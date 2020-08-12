using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL
{
    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal class DefaultExpressionProcessor : ExpressionProcessor
    {
        private readonly Stack<PredicateType> _predicateTypeStack;

        private readonly Stack<String> _parameterNameStack;

        private readonly TemplateBase _templateBase;

        private IReadOnlyList<KeyValuePair<String, String>> _tableAliasMapper;

        /// <summary>
        /// 初始化Parser类的新实例
        /// </summary>
        /// <param name="templateBase"></param>
        public DefaultExpressionProcessor(TemplateBase templateBase, ExpressionProcessorResult expressionProcessorResult)
            : base(expressionProcessorResult)
        {
            Parameter.IfNullOrZero(templateBase);

            _templateBase = templateBase;

            _parameterNameStack = new Stack<String>();
            _predicateTypeStack = new Stack<PredicateType>();

            _tableAliasMapper = new List<KeyValuePair<String, String>>();
        }

        /// <summary>
        /// 执行Expression的解析
        /// </summary>
        /// <param name="expressionStore"></param>
        /// <returns></returns>
        protected override ExpressionProcessorResult InnerProcessor()
        {
            //获取合并后的表别名
            _tableAliasMapper = _expressionStore.MergeAliasMapper();

            //循环翻译连接对象
            foreach (var item in _expressionStore.Joins)
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
                    _expressionProcessorResult.Append(_templateBase.CreateJoin(item.JoinRelation, aliasItem.Key, aliasItem.Value.ToLower()));

                    //获取连接类型中的存储的表达式对象进行翻译
                    InternalParser(item.Expression, item.JoinRelation);
                }
            }
            _expressionProcessorResult.Append(" WHERE 1=1 ");
            //翻译Where条件对象
            if (_expressionStore.Where != null)
            {
                var lambdaExp = (LambdaExpression)_expressionStore.Where.Expression;
                //当表达式主体为常量时则直接返回，不做解析
                if (lambdaExp.Body.NodeType == ExpressionType.Constant)
                {
                    return _expressionProcessorResult;
                }
                _expressionProcessorResult.Append(PredicateType.AND.ToString());

                //获取Where类型中的存储的表达式对象进行翻译
                InternalParser(lambdaExp,JoinRelation.NONE);
            }
            return _expressionProcessorResult;
        }

        /// <summary>
        /// 根据表达式构建出相应结果
        /// </summary>
        /// <param name="expression">表达式</param>
        private void InternalParser(Expression expression, JoinRelation joinRelation)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                    {
                        var binaryExp = (BinaryExpression)expression;

                        if (binaryExp.Left.NodeType != ExpressionType.Constant && binaryExp.Right.NodeType != ExpressionType.Constant)
                        {
                            InternalParser(binaryExp.Left, joinRelation);
                            _expressionProcessorResult.Append(PredicateType.AND.ToString());
                            InternalParser(binaryExp.Right, joinRelation);
                        }
                        else
                        {
                            if (binaryExp.Left.NodeType != ExpressionType.Constant)
                            {
                                InternalParser(binaryExp.Left, joinRelation);
                            }
                            else if (binaryExp.Right.NodeType != ExpressionType.Constant)
                            {
                                InternalParser(binaryExp.Right, joinRelation);
                            }
                        }

                        break;
                    }
                case ExpressionType.OrElse:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        InternalParser(binaryExp.Left, joinRelation);
                        _expressionProcessorResult.Append(PredicateType.OR.ToString());
                        InternalParser(binaryExp.Right, joinRelation);
                        break;
                    }
                case ExpressionType.Call:
                    {
                        ParserMethodCall(expression, joinRelation);
                        break;
                    }
                case ExpressionType.Constant:
                    {
                        var binaryExp = (ConstantExpression)expression;
                        _expressionProcessorResult.Append(new MapperParameter(_parameterNameStack.Pop(), binaryExp.Value));
                        break;
                    }
                case ExpressionType.Equal:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.EQ, joinRelation);
                        break;
                    }
                case ExpressionType.GreaterThan:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.GT, joinRelation);
                        break;
                    }
                case ExpressionType.NotEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.NQ, joinRelation);
                        break;
                    }
                case ExpressionType.GreaterThanOrEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.GE, joinRelation);
                        break;
                    }
                case ExpressionType.LessThan:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.LT, joinRelation);
                        break;
                    }
                case ExpressionType.LessThanOrEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.LE, joinRelation);
                        break;
                    }
                case ExpressionType.Lambda:
                    {
                        var lamdbaExp = (LambdaExpression)expression;
                        if (lamdbaExp.NodeType == ExpressionType.Constant)
                        {
                            break;
                        }

                        if (lamdbaExp.Body is BinaryExpression expression4)
                        {
                            InternalParser(expression4, joinRelation);
                        }
                        else if (lamdbaExp.Body is MemberExpression expression3)
                        {
                            InternalParser(expression3, joinRelation);
                        }
                        else if (lamdbaExp.Body is MethodCallExpression expression2)
                        {
                            InternalParser(expression2, joinRelation);
                        }
                        else if (lamdbaExp.Body is UnaryExpression expression1)
                        {
                            InternalParser(expression1, joinRelation);
                        }
                        break;
                    }
                case ExpressionType.MemberAccess:
                    {
                        var memberExp = (MemberExpression)expression;
                        if (memberExp.Expression.NodeType == ExpressionType.Parameter)
                        {
                            if (_predicateTypeStack.Count == 0)
                            {
                                if (memberExp.Type == typeof(Boolean))
                                {
                                    var parameterExp = (ParameterExpression)memberExp.Expression;
                                    var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberExp.Member.Name)[0]);
                                    var newExpression = Expression.Equal(newMember, Expression.Constant(true));
                                    InternalParser(newExpression, joinRelation);
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
                                _expressionProcessorResult.Append(_templateBase.CreatePredicate(_predicateTypeStack.Pop(), $@"{internalAliasName}{memberExp.Member.Name}", $"@{newParameterName}"));
                                _parameterNameStack.Push(newParameterName);
                            }
                        }
                        else
                        {
                            var getter = Expression.Lambda(memberExp).Compile();
                            _expressionProcessorResult.Append(new MapperParameter(_parameterNameStack.Pop(), getter.DynamicInvoke()));
                        }
                        break;
                    }
                case ExpressionType.Not:
                    {
                        var memberExpression = (MemberExpression)((UnaryExpression)expression).Operand;
                        var parameterExp = (ParameterExpression)memberExpression.Expression;
                        var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberExpression.Member.Name)[0]);
                        InternalParser(Expression.NotEqual(newMember, Expression.Constant(true)), joinRelation);
                        break;
                    }
                case ExpressionType.Convert:
                    {
                        var exp = ((UnaryExpression)expression).Operand;
                        InternalParser(exp, joinRelation);
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
        private void ParserMethodCall(Expression expression, JoinRelation joinRelation)
        {
            Parameter.IfNullOrZero(expression);
            var methodCallExp = (MethodCallExpression)expression;
            var methodName = methodCallExp.Method.Name;

            var methodCallArguments = methodCallExp.Arguments;
            Expression argument;
            Expression obj;
            Type argumentType;
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

            PredicateType predicateType = default;
            if (methodName == "StartsWith")
            {
                predicateType = PredicateType.START_LIKE;
            }
            else if (methodName == "EndsWith")
            {
                predicateType = PredicateType.END_LIKE;
            }
            else if (methodName == "Contains")
            {
                if (argumentType == typeof(String))
                {
                    predicateType = PredicateType.FULL_LIKE;
                }
                else if (argumentType.IsCollection())
                {
                    predicateType = PredicateType.IN;
                }
            }
            else
            {
                throw new Exception("暂不支持的方法");
            }

            _predicateTypeStack.Push(predicateType);

            if (argumentType == typeof(String))
            {
                InternalParser(obj, joinRelation);
                InternalParser(argument, joinRelation);
            }
            else if (argumentType.IsCollection())
            {
                InternalParser(argument, joinRelation);
                InternalParser(obj, joinRelation);
            }
        }

        /// <summary>
        /// 创建谓词语句
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <param name="relationType">关系类型</param>
        private void CreatePredicate(BinaryExpression binary, PredicateType predicateType, JoinRelation joinRelation)
        {
            Parameter.IfNullOrZero(binary);
            var binaryExp = binary;
            if (joinRelation != JoinRelation.NONE)
            {
                GetJoin(binaryExp, predicateType);
            }
            else
            {
                _predicateTypeStack.Push(predicateType);

                //当表达式的左边和右边同时不为常量或者只有左边为常量的话，则从左到右解析表达式，否则从右到左解析表达式
                if ((binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant) || binary.Left.NodeType != ExpressionType.Constant)
                {
                    InternalParser(binaryExp.Left, joinRelation);
                    InternalParser(binaryExp.Right, joinRelation);
                }
                else
                {
                    InternalParser(binaryExp.Right, joinRelation);
                    InternalParser(binaryExp.Left, joinRelation);
                }
            }
        }

        /// <summary>
        /// 将表达式翻译为相应的连接条件
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <param name="relationType">关系类型</param>
        private void GetJoin(BinaryExpression binary, PredicateType predicateType)
        {
            Parameter.IfNullOrZero(binary);

            //表达式左右两边都不为常量时例如 xx.Id==yy.Id
            if (binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant)
            {
                var (LeftMember, LeftAliasName) = ExtractLeftMember(binary);
                var (RightMember, RightAliasName) = ExtractRightMember(binary);

                var relationTemplate = _templateBase.CreatePredicate(predicateType, $"{RightAliasName}.{RightMember.Member.Name}", $"{LeftAliasName}.{LeftMember.Member.Name}");
                _expressionProcessorResult.Append(relationTemplate);
            }
            else if (binary.Left.NodeType == ExpressionType.Constant) //表达式左边为常量
            {
                var (RightMember, RightAliasName) = ExtractRightMember(binary);
                var constant = (ConstantExpression)binary.Left;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _expressionProcessorResult.Append(_templateBase.CreatePredicate(predicateType, value + "", $"{RightAliasName}.{RightMember.Member.Name}"));
            }
            else if (binary.Right.NodeType == ExpressionType.Constant) //表达式的右边为常量
            {
                var (LeftMember, LeftAliasName) = ExtractLeftMember(binary);
                var constant = (ConstantExpression)binary.Right;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _expressionProcessorResult.Append(_templateBase.CreatePredicate(predicateType, $"{LeftAliasName}.{LeftMember.Member.Name}", value + ""));
            }
        }

        /// <summary>
        /// 获取右表达式的成员对象和别名
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <returns></returns>
        private (MemberExpression RightMember, String RightAliasName) ExtractRightMember(BinaryExpression binary)
        {
            Parameter.IfNullOrZero(binary);
            var rightMember = (MemberExpression)binary.Right;
            var rightParameterExp = (ParameterExpression)rightMember.Expression;
            var (tableName, aliasName) = rightParameterExp.Type.GetTableName();
            if (!_tableAliasMapper.Any(a => a.Key == tableName && a.Value == aliasName))
            {
                throw new Exception($@"没有找到参数名:{rightParameterExp.Type.Name}所对应的右表别名");
            }
            var rightAliasName = _tableAliasMapper.Where(w => w.Key == tableName && w.Value == aliasName).FirstOrDefault().Value.ToLower();
            return (rightMember, rightAliasName);
        }

        /// <summary>
        /// 获取左表达式的成员对象和别名
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <returns></returns>
        private (MemberExpression LeftMember, String LeftAliasName) ExtractLeftMember(BinaryExpression binary)
        {
            Parameter.IfNullOrZero(binary);
            var leftMember = (MemberExpression)binary.Left;
            var leftParameterExp = (ParameterExpression)leftMember.Expression;
            var (tableName, aliasName) = leftParameterExp.Type.GetTableName();
            if (!_tableAliasMapper.Any(a => a.Key == tableName && a.Value == aliasName))
            {
                throw new Exception($@"没有找到参数类型:{leftParameterExp.Type.Name}所对应的左表别名");
            }
            var leftAliasName = _tableAliasMapper.Where(w => w.Key == tableName && w.Value == aliasName).FirstOrDefault().Value.ToLower();
            return (leftMember, leftAliasName);
        }
    }
}
