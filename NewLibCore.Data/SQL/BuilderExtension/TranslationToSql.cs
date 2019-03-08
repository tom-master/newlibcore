using NewLibCore.Data.SQL.InternalDataStore;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal class TranslationToSql : ITranslate
    {
        internal SqlTemporaryStore TemporaryStore { get; private set; }

        private DatabaseSyntaxBuilder _syntaxBuilder = new MysqlSyntaxBuilder();

        private Stack<String> _parameterNameStack;

        private Stack<RelationType> _operationalCharacterStack;

        private JoinType _joinType;

        private readonly IDictionary<String, String> _expressionParameterNameToTableAliasNameMappers;


        public TranslationToSql()
        {
            TemporaryStore = new SqlTemporaryStore();
            _operationalCharacterStack = new Stack<RelationType>();
            _parameterNameStack = new Stack<String>();
            _expressionParameterNameToTableAliasNameMappers = new Dictionary<String, String>();
        }

        public SqlTemporaryStore Translate(Expression expression, JoinType joinType = JoinType.None, Boolean alias = false)
        {
            if (joinType != JoinType.None)
            {
                var lamdbaExp = (LambdaExpression)expression;
                var aliasName = lamdbaExp.Parameters[0].Type.Name;
                InitExpressionParameterMapper(lamdbaExp.Parameters);
                TemporaryStore.Append($@"{joinType.GetDescription()} {aliasName}");
                if (alias)
                {
                    TemporaryStore.Append($@" AS {aliasName.ToLower()} ON ");
                }
                _joinType = joinType;
            }
            else
            {
                TemporaryStore.Append(" WHERE ");
            }
            InternalBuildWhere(expression);

            return TemporaryStore;
        }

        private void InitExpressionParameterMapper(IList<ParameterExpression> parameters)
        {
            foreach (var item in parameters)
            {
                _expressionParameterNameToTableAliasNameMappers.Add(item.Name, item.Type.Name.ToLower());
            }
        }

        private void InternalBuildWhere(Expression expression)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    TemporaryStore.Append(RelationType.AND.ToString());
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.OrElse:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    TemporaryStore.Append(RelationType.OR.ToString());
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
                    TemporaryStore.AppendParameter(new SqlParameterMapper($@"@{_parameterNameStack.Pop()}", binaryExp.Value));
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
                            TemporaryStore.Append(syntax);
                            _parameterNameStack.Push(newParameterName);
                        }
                    }
                    else
                    {
                        var getter = Expression.Lambda(memberExp).Compile();
                        Object result = result = getter.DynamicInvoke();
                        TemporaryStore.AppendParameter(new SqlParameterMapper($@"@{_parameterNameStack.Pop()}", result));
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
                    break;
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
            else if (argumentType == typeof(Int32[]) || (argumentType.Name == "List`1" || argumentType.Name == "IList`1"))
            {
                InternalBuildWhere(argument);
                InternalBuildWhere(obj);
            }
        }

        private void LogicStatementBuilder(BinaryExpression binary, RelationType relationType)
        {
            var binaryExp = binary;
            if (_joinType != JoinType.None)
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
            var leftAliasName = _expressionParameterNameToTableAliasNameMappers[((ParameterExpression)leftMemberExp.Expression).Name];

            if (binaryExp.Right.GetType() == typeof(ConstantExpression))
            {
                var constant = (ConstantExpression)binaryExp.Right;
                Boolean result;
                if (Boolean.TryParse(constant.Value.ToString(), out result))
                {
                    TemporaryStore.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {(result ? 1 : 0)} ");
                }
                else
                {
                    TemporaryStore.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {constant.Value} ");
                }
            }
            else
            {
                var rightMemberExp = (MemberExpression)binaryExp.Right;
                var rightAliasName = _expressionParameterNameToTableAliasNameMappers[((ParameterExpression)rightMemberExp.Expression).Name];

                TemporaryStore.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {rightAliasName}.{rightMemberExp.Member.Name} ");
            }
        }
    }

    internal class SqlTemporaryStore
    {
        internal StringBuilder SqlStore { get; private set; }

        internal IList<SqlParameterMapper> ParameterStore { get; private set; }

        internal SqlTemporaryStore()
        {
            SqlStore = new StringBuilder();
            ParameterStore = new List<SqlParameterMapper>();
        }

        internal void Append(String sql)
        {
            SqlStore.Append(sql);
        }

        internal void AppendParameter(params SqlParameterMapper[] mapper)
        {
            foreach (var item in mapper)
            {
                ParameterStore.Add(item);
            }
        }

        internal void Clear()
        {
            SqlStore.Clear();
            ParameterStore.Clear();
        }
    }
}
