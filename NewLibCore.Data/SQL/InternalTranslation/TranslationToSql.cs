using NewLibCore.Data.SQL.InternalExecute;
using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Data.SQL.InternalTranslation
{
    internal class TranslationToSql : ITranslate
    {
        internal TranslationResult TranslationResult { get; private set; }

        private DatabaseSyntaxBuilder _syntaxBuilder = SwitchDatabase.DatabaseSyntax;

        private Stack<String> _parameterNameStack;

        private Stack<RelationType> _operationalCharacterStack;

        private JoinType _joinType;

        private readonly IDictionary<String, String> _parameterToTableAliasMappers;

        public TranslationToSql()
        {
            TranslationResult = new TranslationResult();
            _operationalCharacterStack = new Stack<RelationType>();
            _parameterNameStack = new Stack<String>();
            _parameterToTableAliasMappers = new Dictionary<String, String>();
        }

        public TranslationResult Translate(StatementStore statementStore)
        {
            var lamdbaExp = (LambdaExpression)statementStore.Expression;
            foreach (var item in statementStore.JoinStores)
            {
                foreach (var parameter in ((LambdaExpression)item.Expression).Parameters)
                {
                    InitExpressionParameterMapper(new KeyValuePair<String, String>(parameter.Name, parameter.Type.Name.ToLower()));
                }

                foreach (var parameter in item.AliasNameMappers)
                {
                    TranslationResult.Append($@" {item.JoinType.GetDescription()} {parameter.Value} AS {parameter.Value.ToLower()} ON ");
                }
                _joinType = item.JoinType;
                InternalBuildWhere(item.Expression);
                _parameterToTableAliasMappers.Clear();
            }

            if (statementStore.Expression != null)
            {
                _joinType = JoinType.NONE;
                TranslationResult.Append($@" WHERE {(statementStore.AliasName == null ? "" : $@"{statementStore.AliasName}.")}");
                InternalBuildWhere(statementStore.Expression);
            }

            return TranslationResult;
        }

        private void InternalBuildWhere(Expression expression)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    TranslationResult.Append(RelationType.AND.ToString());
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.OrElse:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    TranslationResult.Append(RelationType.OR.ToString());
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.Call:
                {
                    MethodCall(expression);
                    break;
                }
                case ExpressionType.Constant:
                {
                    var binaryExp = (ConstantExpression)expression;
                    TranslationResult.AppendParameter(new EntityParameter($@"@{_parameterNameStack.Pop()}", binaryExp.Value));
                    break;
                }
                case ExpressionType.Equal:
                {
                    var binaryExp = (BinaryExpression)expression;
                    LogicStatementBuilder(binaryExp, RelationType.EQ);
                    break;
                }
                case ExpressionType.GreaterThan:
                {
                    var binaryExp = (BinaryExpression)expression;
                    LogicStatementBuilder(binaryExp, RelationType.GT);
                    break;
                }
                case ExpressionType.NotEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    LogicStatementBuilder(binaryExp, RelationType.NQ);
                    break;
                }
                case ExpressionType.GreaterThanOrEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    LogicStatementBuilder(binaryExp, RelationType.GE);
                    break;
                }
                case ExpressionType.LessThan:
                {
                    var binaryExp = (BinaryExpression)expression;
                    LogicStatementBuilder(binaryExp, RelationType.LT);
                    break;
                }
                case ExpressionType.LessThanOrEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    LogicStatementBuilder(binaryExp, RelationType.LE);
                    break;
                }
                case ExpressionType.Lambda:
                {
                    var lamdbaExp = (LambdaExpression)expression;
                    if (lamdbaExp.Body is BinaryExpression)
                    {
                        InternalBuildWhere((BinaryExpression)lamdbaExp.Body);
                    }
                    else if (lamdbaExp.Body is MemberExpression)
                    {
                        InternalBuildWhere((MemberExpression)lamdbaExp.Body);
                    }
                    else if (lamdbaExp.Body is MethodCallExpression)
                    {
                        InternalBuildWhere((MethodCallExpression)lamdbaExp.Body);
                    }
                    else
                    {
                        InternalBuildWhere((UnaryExpression)lamdbaExp.Body);
                    }
                    break;
                }
                case ExpressionType.MemberAccess:
                {
                    var memberExp = (MemberExpression)expression;
                    var memberName = memberExp.Member.Name;

                    var newParameterName = $@"{Guid.NewGuid().ToString().Replace("-", "")}";
                    if (memberExp.Expression.NodeType == ExpressionType.Parameter)
                    {
                        if (_operationalCharacterStack.Count == 0)
                        {
                            if (memberExp.Type == typeof(Boolean))
                            {
                                var parameterExp = (ParameterExpression)memberExp.Expression;
                                var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberName)[0]);
                                var newExpression = Expression.Equal(newMember, Expression.Constant(true));
                                InternalBuildWhere(newExpression);
                            }
                        }
                        else
                        {
                            var syntax = _syntaxBuilder.SyntaxBuilder(_operationalCharacterStack.Pop(), memberName, newParameterName);
                            TranslationResult.Append(syntax);
                            _parameterNameStack.Push(newParameterName);
                        }
                    }
                    else
                    {
                        var getter = Expression.Lambda(memberExp).Compile();
                        TranslationResult.AppendParameter(new EntityParameter($@"@{_parameterNameStack.Pop()}", getter.DynamicInvoke()));
                        break;
                    }
                    break;
                }
                case ExpressionType.Not:
                {
                    var memberExpression = (MemberExpression)((UnaryExpression)expression).Operand;
                    var parameterExp = (ParameterExpression)memberExpression.Expression;
                    var memberName = memberExpression.Member.Name;
                    var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberName)[0]);
                    var newExpression = Expression.NotEqual(newMember, Expression.Constant(true));
                    InternalBuildWhere(newExpression);
                    break;
                }
                default:
                    throw new NotSupportedException($@"暂不支持的表达式操作:{expression.NodeType}");
            }
        }

        private void MethodCall(Expression expression)
        {
            var methodCallExp = (MethodCallExpression)expression;
            var methodName = methodCallExp.Method.Name;

            var methodCallArguments = methodCallExp.Arguments;
            Type argumentType = null;
            Expression argument = null;
            Expression obj = null;
            if (methodCallArguments.Count > 1)
            {
                argumentType = methodCallArguments[0].Type;
                argument = methodCallArguments[1];
                obj = methodCallArguments[0];
            }
            else
            {
                argumentType = methodCallExp.Object.Type;
                argument = methodCallArguments[0];
                obj = methodCallExp.Object;
            }

            var relationType = default(RelationType);
            if (methodName == "StartsWith")
            {
                relationType = RelationType.START_LIKE;
            }
            else if (methodName == "EndsWith")
            {
                relationType = RelationType.END_LIKE;
            }
            else if (methodName == "Contains")
            {
                if (argumentType == typeof(String))
                {
                    relationType = RelationType.LIKE;
                }
                else if (argumentType == typeof(Int32[]) || (argumentType.Name == "List`1" || argumentType.Name == "IList`1"))
                {
                    relationType = RelationType.IN;
                }
            }
            else
            {
                throw new Exception("暂不支持的方法");
            }

            _operationalCharacterStack.Push(relationType);

            if (argumentType == typeof(String))
            {
                InternalBuildWhere(obj);
                InternalBuildWhere(argument);
            }
            else if ((((TypeInfo)argumentType).ImplementedInterfaces as IList<Type>).Any(face => face == typeof(IList) || face == typeof(ICollection) || face == typeof(IEnumerable)))
            {
                InternalBuildWhere(argument);
                InternalBuildWhere(obj);
            }
        }

        private void LogicStatementBuilder(BinaryExpression binary, RelationType relationType)
        {
            var binaryExp = binary;
            if (_joinType != JoinType.NONE)
            {
                GetJoin(binaryExp, relationType);
            }
            else
            {
                _operationalCharacterStack.Push(relationType);
                InternalBuildWhere(binaryExp.Left);
                InternalBuildWhere(binaryExp.Right);
            }
        }

        private void GetJoin(BinaryExpression binaryExp, RelationType relationType)
        {
            var leftMemberExp = (MemberExpression)binaryExp.Left;
            var leftAliasName = _parameterToTableAliasMappers[((ParameterExpression)leftMemberExp.Expression).Name];

            if (binaryExp.Right.GetType() == typeof(ConstantExpression))
            {
                var constant = (ConstantExpression)binaryExp.Right;
                if (Boolean.TryParse(constant.Value.ToString(), out var result))
                {
                    TranslationResult.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {(result ? 1 : 0)} ");
                }
                else
                {
                    TranslationResult.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {constant.Value} ");
                }
            }
            else
            {
                var rightMemberExp = (MemberExpression)binaryExp.Right;
                var rightAliasName = _parameterToTableAliasMappers[((ParameterExpression)rightMemberExp.Expression).Name];

                TranslationResult.Append($@" {rightAliasName}.{rightMemberExp.Member.Name} {relationType.GetDescription()} {leftAliasName}.{leftMemberExp.Member.Name}");
            }
        }

        private void InitExpressionParameterMapper(params KeyValuePair<String, String>[] keyValuePairs)
        {
            foreach (var item in keyValuePairs)
            {
                _parameterToTableAliasMappers.Add(item.Key, item.Value);
            }
        }

    }
}
