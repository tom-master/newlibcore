using System;
using System.Collections;
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
        private JoinType _joinType;

        private readonly Stack<String> _parameterNameStack;

        private readonly Stack<RelationType> _relationTypesStack;

        private readonly TranslationStatementStore _statementStore;

        private IReadOnlyList<KeyValuePair<String, String>> _tableAliasMapper;

        internal TranslationCore(TranslationStatementStore statementStore)
        {
            Parameter.Validate(statementStore);

            _statementStore = statementStore;

            _relationTypesStack = new Stack<RelationType>();
            _parameterNameStack = new Stack<String>();
            _tableAliasMapper = new List<KeyValuePair<String, String>>();

            Result = new TranslationCoreResult();
        }

        internal TranslationCoreResult Result { get; private set; }

        public TranslationCoreResult Translate()
        {
            _tableAliasMapper = _statementStore.MergeAliasMapper();
            foreach (var item in _statementStore.Joins)
            {
                if (item.AliaNameMapper == null || item.JoinType == JoinType.NONE)
                {
                    continue;
                }

                foreach (var aliasItem in item.AliaNameMapper)
                {
                    if (aliasItem.Value.ToLower() == item.MainTable.ToLower())
                    {
                        continue;
                    }

                    var joinTemplate = MapperFactory.Instance.JoinBuilder(item.JoinType, aliasItem.Value, aliasItem.Value.ToLower());
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
                    if (binaryExp.Right.NodeType == ExpressionType.Constant)
                    {
                        break;
                    }

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
                            var internalAliasName = "";
                            if (_statementStore.ExecuteType != ExecuteType.UPDATE)
                            {
                                if (!_tableAliasMapper.Any(a => a.Key == parameterExp.Name && a.Value == parameterExp.Type.GetAliasName()))
                                {
                                    throw new ArgumentException($@"没有找到{parameterExp.Type.Name}所对应的形参");
                                }
                                internalAliasName = $@"{ _tableAliasMapper.Where(w => w.Key == parameterExp.Name && w.Value == parameterExp.Type.GetAliasName()).FirstOrDefault().Value.ToLower()}.";
                            }

                            var newParameterName = MapperFactory.Instance.UnionPlaceHolder;
                            Result.Append(MapperFactory.Instance.RelationBuilder(_relationTypesStack.Pop(), $@"{internalAliasName}{memberExp.Member.Name}", $"@{newParameterName}"));
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
                    var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberExpression.Member.Name)[0]);
                    InternalBuildWhere(Expression.NotEqual(newMember, Expression.Constant(true)));
                    break;
                }
                case ExpressionType.Convert:
                {
                    var exp = ((UnaryExpression)expression).Operand;
                    InternalBuildWhere(exp);
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

            RelationType relationType = default;
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
                else if (argumentType.GetInterfaces().Any(a => a == typeof(IEnumerable)))
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
            else if (argumentType.GetInterfaces().Any(a => a == typeof(IEnumerable)))
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
            Parameter.Validate(binaryExp);

            var leftMember = (MemberExpression)binaryExp.Left;

            ParameterExpression leftParameterExp = null;
            if (leftMember.Expression.NodeType == ExpressionType.Parameter)
            {
                leftParameterExp = (ParameterExpression)leftMember.Expression;
            }

            if (!_tableAliasMapper.Any(a => a.Key == leftParameterExp.Name && a.Value == leftParameterExp.Type.GetAliasName()))
            {
                throw new Exception($@"没有找到参数名:{leftParameterExp.Name}所对应的左表别名");
            }

            var leftAliasName = _tableAliasMapper.Where(w => w.Key == leftParameterExp.Name && w.Value == leftParameterExp.Type.GetAliasName()).FirstOrDefault().Value.ToLower();
            if (binaryExp.Right.GetType() != typeof(ConstantExpression))
            {
                var rightMember = (MemberExpression)binaryExp.Right;
                var rightParameterExp = (ParameterExpression)rightMember.Expression;
                if (!_tableAliasMapper.Any(a => a.Key == rightParameterExp.Name && a.Value == rightParameterExp.Type.GetAliasName()))
                {
                    throw new Exception($@"没有找到参数名:{rightParameterExp.Name}所对应的右表别名");
                }
                var rightAliasName = _tableAliasMapper.Where(w => w.Key == rightParameterExp.Name && w.Value == rightParameterExp.Type.GetAliasName()).FirstOrDefault().Value.ToLower();
                var relationTemplate = MapperFactory.Instance.RelationBuilder(relationType, $"{rightAliasName}.{rightMember.Member.Name}", $"{leftAliasName}.{leftMember.Member.Name}");
                Result.Append(relationTemplate);
                return;
            }

            var constant = (ConstantExpression)binaryExp.Right;
            var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
            Result.Append(MapperFactory.Instance.RelationBuilder(relationType, $"{leftAliasName}.{leftMember.Member.Name}", value));
        }
    }
}
