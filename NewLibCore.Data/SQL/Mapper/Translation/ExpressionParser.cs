using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 翻译表达式
    /// </summary>
    internal interface IExpressionParser
    {
        /// <summary>
        /// 翻译
        /// </summary>
        /// <returns></returns>
        StringBuilder Parse(ExpressionStore expressionStore);
    }

    /// <summary>
    /// 翻译表达式
    /// </summary>
    internal class ExpressionParser : IExpressionParser
    {
        private JoinRelation _joinRelation;

        private readonly StringBuilder _internalStore;

        private readonly Stack<String> _parameterNameStack;

        private readonly Stack<RelationType> _relationTypesStack;

        private IReadOnlyList<KeyValuePair<String, String>> _tableAliasMapper;

        /// <summary>
        /// 初始化一个TranslateContext类的实例
        /// </summary>
        /// <param name="statementStore">表达式分解后的对象</param>
        /// <returns></returns>
        private ExpressionParser()
        {
            _internalStore = new StringBuilder();
            _parameterNameStack = new Stack<String>();
            _relationTypesStack = new Stack<RelationType>();
            _tableAliasMapper = new List<KeyValuePair<String, String>>();
        }

        /// <summary>
        /// 初始化一个ExpressionParser类的实例
        /// </summary>
        /// <returns></returns>
        internal static ExpressionParser CreateParser()
        {
            return new ExpressionParser();
        }

        /// <summary>
        /// 翻译
        /// </summary>
        /// <returns></returns>
        public StringBuilder Parse(ExpressionStore expressionStore)
        {
            Parameter.Validate(expressionStore);

            //获取合并后的表别名
            _tableAliasMapper = expressionStore.MergeAliasMapper();

            //循环翻译连接对象
            foreach (var item in expressionStore.Joins)
            {
                if (item.AliaNameMapper == null || item.JoinRelation == JoinRelation.NONE)
                {
                    continue;
                }

                //获取连接对象中的表别名，进行连接语句的翻译
                foreach (var aliasItem in item.AliaNameMapper)
                {
                    if (aliasItem.Key.ToLower() == item.MainTable.ToLower())
                    {
                        continue;
                    }

                    //获取连接语句的模板
                    var joinTemplate = MapperConfig.Instance.JoinBuilder(item.JoinRelation, aliasItem.Key, aliasItem.Value.ToLower());
                    _internalStore.Append(joinTemplate);

                    //设置相应的连接类型
                    _joinRelation = item.JoinRelation;

                    //获取连接类型中的存储的表达式对象进行翻译
                    InternalBuildWhere(item.Expression);
                }
            }
            _internalStore.Append("WHERE 1=1");
            //翻译Where条件对象
            if (expressionStore.Where != null)
            {
                var lambdaExp = (LambdaExpression)expressionStore.Where.Expression;
                //当表达式主体为常量时则直接返回，不做解析
                if (lambdaExp.Body.NodeType == ExpressionType.Constant)
                {
                    return _internalStore;
                }

                _joinRelation = JoinRelation.NONE;
                _internalStore.Append(RelationType.AND.ToString());

                //获取Where类型中的存储的表达式对象进行翻译
                InternalBuildWhere(lambdaExp);
            }
            return _internalStore;
        }

        /// <summary>
        /// 根据表达式构建出相应结果
        /// </summary>
        /// <param name="expression">表达式</param>
        private void InternalBuildWhere(Expression expression)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                    {
                        var binaryExp = (BinaryExpression)expression;

                        if (binaryExp.Left.NodeType != ExpressionType.Constant && binaryExp.Right.NodeType != ExpressionType.Constant)
                        {
                            InternalBuildWhere(binaryExp.Left);
                            _internalStore.Append(RelationType.AND.ToString());
                            InternalBuildWhere(binaryExp.Right);
                        }
                        else
                        {
                            if (binaryExp.Left.NodeType != ExpressionType.Constant)
                            {
                                InternalBuildWhere(binaryExp.Left);
                            }
                            else if (binaryExp.Right.NodeType != ExpressionType.Constant)
                            {
                                InternalBuildWhere(binaryExp.Right);
                            }
                        }

                        break;
                    }
                case ExpressionType.OrElse:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        InternalBuildWhere(binaryExp.Left);
                        _internalStore.Append(RelationType.OR.ToString());
                        InternalBuildWhere(binaryExp.Right);
                        break;
                    }
                case ExpressionType.Call:
                    {
                        TranslateMethodCall(expression);
                        break;
                    }
                case ExpressionType.Constant:
                    {
                        var binaryExp = (ConstantExpression)expression;
                        _internalStore.Append(new EntityParameter(_parameterNameStack.Pop(), binaryExp.Value));
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
                        if (lamdbaExp.NodeType == ExpressionType.Constant)
                        {
                            break;
                        }

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
                        else if (lamdbaExp.Body is UnaryExpression)
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
                                if (!_tableAliasMapper.Any(a => a.Key == parameterExp.Type.GetTableName().TableName && a.Value == parameterExp.Type.GetTableName().AliasName))
                                {
                                    throw new ArgumentException($@"没有找到{parameterExp.Type.Name}所对应的形参");
                                }
                                internalAliasName = $@"{ _tableAliasMapper.Where(w => w.Key == parameterExp.Type.GetTableName().TableName && w.Value == parameterExp.Type.GetTableName().AliasName).FirstOrDefault().Value.ToLower()}.";

                                var newParameterName = Guid.NewGuid().ToString().Replace("-", "");
                                _internalStore.Append(MapperConfig.Instance.RelationBuilder(_relationTypesStack.Pop(), $@"{internalAliasName}{memberExp.Member.Name}", $"@{newParameterName}"));
                                _parameterNameStack.Push(newParameterName);
                            }
                        }
                        else
                        {
                            var getter = Expression.Lambda(memberExp).Compile();
                            _internalStore.Append(new EntityParameter(_parameterNameStack.Pop(), getter.DynamicInvoke()));
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
                    {
                        throw new NotSupportedException($@"暂不支持的表达式操作:{expression.NodeType}");
                    }
            }
        }

        /// <summary>
        /// 翻译表达式中的方法调用
        /// </summary>
        /// <param name="expression">表达式</param>
        private void TranslateMethodCall(Expression expression)
        {
            var methodCallExp = (MethodCallExpression)expression;
            var methodName = methodCallExp.Method.Name;

            var methodCallArguments = methodCallExp.Arguments;
            Type argumentType = null;
            Expression argument = null, obj = null;
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
                else if (argumentType.IsCollections())
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
            else if (argumentType.IsCollections())
            {
                InternalBuildWhere(argument);
                InternalBuildWhere(obj);
            }
        }

        /// <summary>
        /// 逻辑语句构建
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <param name="relationType">关系类型</param>
        private void LogicStatementBuilder(BinaryExpression binary, RelationType relationType)
        {
            var binaryExp = binary;
            if (_joinRelation != JoinRelation.NONE)
            {
                GetJoin(binaryExp, relationType);
            }
            else
            {
                _relationTypesStack.Push(relationType);

                //当表达式的左边和右边同时不为常量或者只有左边为常量的话，则从左到右解析表达式，否则从右到左解析表达式
                if ((binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant) || binary.Left.NodeType != ExpressionType.Constant)
                {
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
                }
                else
                {
                    InternalBuildWhere(binaryExp.Right);
                    InternalBuildWhere(binaryExp.Left);
                }
            }
        }

        /// <summary>
        /// 将表达式翻译为相应的连接条件
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <param name="relationType">关系类型</param>
        private void GetJoin(BinaryExpression binary, RelationType relationType)
        {
            Parameter.Validate(binary);

            //表达式左右两边都不为常量时例如 xx.Id==yy.Id
            if (binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant)
            {
                var (LeftMember, LeftAliasName) = GetLeftMemberAndAliasName(binary);
                var (RightMember, RightAliasName) = GetRightMemberAndAliasName(binary);

                var relationTemplate = MapperConfig.Instance.RelationBuilder(relationType, $"{RightAliasName}.{RightMember.Member.Name}", $"{LeftAliasName}.{LeftMember.Member.Name}");
                _internalStore.Append(relationTemplate);
            }
            else if (binary.Left.NodeType == ExpressionType.Constant) //表达式左边为常量
            {
                var (RightMember, RightAliasName) = GetRightMemberAndAliasName(binary);
                var constant = (ConstantExpression)binary.Left;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _internalStore.Append(MapperConfig.Instance.RelationBuilder(relationType, value + "", $"{RightAliasName}.{RightMember.Member.Name}"));
            }
            else if (binary.Right.NodeType == ExpressionType.Constant) //表达式的右边为常量
            {
                var (LeftMember, LeftAliasName) = GetLeftMemberAndAliasName(binary);
                var constant = (ConstantExpression)binary.Right;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _internalStore.Append(MapperConfig.Instance.RelationBuilder(relationType, $"{LeftAliasName}.{LeftMember.Member.Name}", value + ""));
            }
        }

        /// <summary>
        /// 获取左表达式的成员对象和别名
        /// </summary>
        /// <param name="binaryExp">表达式</param>
        /// <returns></returns>
        private (MemberExpression RightMember, String RightAliasName) GetRightMemberAndAliasName(BinaryExpression binaryExp)
        {
            var rightMember = (MemberExpression)binaryExp.Right;
            var rightParameterExp = (ParameterExpression)rightMember.Expression;
            if (!_tableAliasMapper.Any(a => a.Key == rightParameterExp.Type.GetTableName().TableName && a.Value == rightParameterExp.Type.GetTableName().AliasName))
            {
                throw new Exception($@"没有找到参数名:{rightParameterExp.Type.Name}所对应的右表别名");
            }
            var rightAliasName = _tableAliasMapper.Where(w => w.Key == rightParameterExp.Type.GetTableName().TableName && w.Value == rightParameterExp.Type.GetTableName().AliasName).FirstOrDefault().Value.ToLower();
            return (rightMember, rightAliasName);
        }

        /// <summary>
        /// 获取又表达式的成员对象和别名
        /// </summary>
        /// <param name="binaryExp">表达式</param>
        /// <returns></returns>
        private (MemberExpression LeftMember, String LeftAliasName) GetLeftMemberAndAliasName(BinaryExpression binaryExp)
        {
            var leftMember = (MemberExpression)binaryExp.Left;
            var leftParameterExp = (ParameterExpression)leftMember.Expression;
            if (!_tableAliasMapper.Any(a => a.Key == leftParameterExp.Type.GetTableName().TableName && a.Value == leftParameterExp.Type.GetTableName().AliasName))
            {
                throw new Exception($@"没有找到参数类型:{leftParameterExp.Type.Name}所对应的左表别名");
            }
            var leftAliasName = _tableAliasMapper.Where(w => w.Key == leftParameterExp.Type.GetTableName().TableName && w.Value == leftParameterExp.Type.GetTableName().AliasName).FirstOrDefault().Value.ToLower();
            return (leftMember, leftAliasName);
        }
    }
}
