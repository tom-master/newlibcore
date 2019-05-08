using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
    internal class TranslationCore : ITranslateCore
    {
        private readonly Stack<String> _parameterNameStack;

        private readonly Stack<RelationType> _relationTypesStack;

        private readonly StatementStore _statementStore;

        private JoinType _joinType;

        private IDictionary<String, String> _tableAliasMapper;

        public TranslationCore(StatementStore statementStore)
        {
            Parameter.Validate(statementStore);

            _statementStore = statementStore;

            _relationTypesStack = new Stack<RelationType>();
            _parameterNameStack = new Stack<String>();
            _tableAliasMapper = new Dictionary<String, String>();

            Result = new TranslationCoreResult();
        }

        internal TranslationCoreResult Result { get; private set; }

        public TranslationCoreResult Translate()
        {
            _tableAliasMapper.Clear();
            _tableAliasMapper = _statementStore.MergeAliasMapper().ToDictionary(d => d.Key, d => d.Value);
            foreach (var item in _statementStore.Joins)
            {
                if (item.AliaNameMapper == null)
                {
                    continue;
                }

                foreach (var aliasItem in item.AliaNameMapper)
                {
                    if (aliasItem.Value.ToLower() == item.MainTable.ToLower())
                    {
                        continue;
                    }

                    var joinTemplate = MapperFactory.Mapper.JoinBuilder(item.JoinType, aliasItem.Value, aliasItem.Value.ToLower());
                    Result.Append(joinTemplate);
                    _joinType = item.JoinType;
                    InternalBuildWhere(item.Expression);
                }
            }
            Result.Append("WHERE 1=1");
            if (_statementStore.Where != null)
            {
                _joinType = JoinType.NONE;
                Result.Append(RelationType.AND.ToString());
                InternalBuildWhere(_statementStore.Where.Expression);
            }

            return Result;
        }

        private void InternalBuildWhere(Expression expression)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    Result.Append(RelationType.AND.ToString());
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.OrElse:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    Result.Append(RelationType.OR.ToString());
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
                    Result.Append(new EntityParameter($@"@{_parameterNameStack.Pop()}", binaryExp.Value));
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

                    var newParameterName = $@"{Guid.NewGuid().ToString().Replace("-", "")}";
                    if (memberExp.Expression.NodeType == ExpressionType.Parameter)
                    {
                        if (_relationTypesStack.Count == 0)
                        {
                            if (memberExp.Type == typeof(Boolean))
                            {
                                var parameterExp = (ParameterExpression)memberExp.Expression;
                                var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberExp.Member.Name)[0]);
                                var newExpression = Expression.Equal(newMember, Expression.Constant(true));
                                InternalBuildWhere(newExpression);
                            }
                        }
                        else
                        {
                            var parameterExp = (ParameterExpression)memberExp.Expression;
                            if (!_tableAliasMapper.ContainsKey(parameterExp.Name))
                            {
                                throw new ArgumentException($@"没有找到{parameterExp.Type.Name}所对应的形参");
                            }

                            var syntax = MapperFactory.Mapper.RelationBuilder(_relationTypesStack.Pop(), $@"{_tableAliasMapper[parameterExp.Name].ToLower()}.{memberExp.Member.Name}", $"@{newParameterName}");
                            Result.Append(syntax);
                            _parameterNameStack.Push(newParameterName);
                        }
                    }
                    else
                    {
                        var getter = Expression.Lambda(memberExp).Compile();
                        Result.Append(new EntityParameter($@"@{_parameterNameStack.Pop()}", getter.DynamicInvoke()));
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
                    relationType = RelationType.FULL_LIKE;
                }
                else if (argumentType.GetGenericTypeDefinition() == typeof(List<>))
                {
                    relationType = RelationType.IN;
                }
            }
            else
            {
                throw new Exception("暂不支持的方法");
            }

            _relationTypesStack.Push(relationType);

            if (argumentType == typeof(String))
            {
                InternalBuildWhere(obj);
                InternalBuildWhere(argument);
            }
            else if (argumentType.GetGenericTypeDefinition() == typeof(List<>))
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
                _relationTypesStack.Push(relationType);
                InternalBuildWhere(binaryExp.Left);
                InternalBuildWhere(binaryExp.Right);
            }
        }

        private void GetJoin(BinaryExpression binaryExp, RelationType relationType)
        {
            var leftMember = (MemberExpression)binaryExp.Left;
            var leftParameterName = ((ParameterExpression)leftMember.Expression).Name;
            if (!_tableAliasMapper.ContainsKey(leftParameterName))
            {
                throw new Exception($@"没有找到参数名:{leftParameterName}所对应的左表别名");
            }

            var leftAliasName = _tableAliasMapper[leftParameterName].ToLower();

            if (binaryExp.Right.GetType() == typeof(ConstantExpression))
            {
                var constant = (ConstantExpression)binaryExp.Right;
                if (Boolean.TryParse(constant.Value.ToString(), out var result))
                {
                    var relationTemplate = MapperFactory.Mapper.RelationBuilder(relationType, $"{leftAliasName}.{leftMember.Member.Name}", (result ? 1 : 0).ToString());
                    Result.Append(relationTemplate);
                }
                else
                {
                    var relationTemplate = MapperFactory.Mapper.RelationBuilder(relationType, $"{leftAliasName}.{leftMember.Member.Name}", constant.Value);
                    Result.Append(relationTemplate);
                }
            }
            else
            {
                var rightMember = (MemberExpression)binaryExp.Right;
                var rightParameterName = ((ParameterExpression)rightMember.Expression).Name;
                if (!_tableAliasMapper.ContainsKey(rightParameterName))
                {
                    throw new Exception($@"没有找到参数名:{leftParameterName}所对应的右表别名");
                }

                var rightAliasName = _tableAliasMapper[rightParameterName].ToLower();
                var relationTemplate = MapperFactory.Mapper.RelationBuilder(relationType, $"{rightAliasName}.{rightMember.Member.Name}", $"{leftAliasName}.{leftMember.Member.Name}");
                Result.Append(relationTemplate);
            }
        }

    }
}
