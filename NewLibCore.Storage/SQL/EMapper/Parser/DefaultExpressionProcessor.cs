using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal class DefaultExpressionProcessor : ExpressionProcessor
    {
        private readonly Stack<PredicateType> _predicateTypeStack;

        private readonly Stack<String> _parameterNameStack;

        private readonly TemplateBase _template;

        private IReadOnlyList<KeyValuePair<String, String>> _aliasMapper;

        /// <summary>
        /// 初始化Parser类的新实例
        /// </summary>
        /// <param name="templateBase"></param>
        public DefaultExpressionProcessor(TemplateBase template, ProcessorResult processorResult)
            : base(processorResult)
        {
            Check.IfNullOrZero(template);

            _template = template;

            _parameterNameStack = new Stack<String>();
            _predicateTypeStack = new Stack<PredicateType>();

            _aliasMapper = new List<KeyValuePair<String, String>>();
        }

        /// <summary>
        /// 执行Expression的解析
        /// </summary>
        /// <param name="expressionStore"></param>
        /// <returns></returns>
        protected override ProcessorResult InnerProcessor()
        {
            //获取合并后的表别名
            _aliasMapper = _expressionStore.MergeAliasMapper();

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
                    _processorResult.Append(_template.CreateJoin(item.JoinRelation, aliasItem.Key, aliasItem.Value.ToLower()));

                    //获取连接类型中的存储的表达式对象进行翻译
                    InternalParser(item.Expression, item.JoinRelation);
                }
            }
            _processorResult.Append(" WHERE 1=1 ");
            //翻译Where条件对象
            if (_expressionStore.Where != null)
            {
                var lambdaExp = (LambdaExpression)_expressionStore.Where.Expression;
                //当表达式主体为常量时则直接返回，不做解析
                if (lambdaExp.Body.NodeType == ExpressionType.Constant)
                {
                    return _processorResult;
                }
                _processorResult.Append(PredicateType.AND.ToString());

                //获取Where类型中的存储的表达式对象进行翻译
                InternalParser(lambdaExp, JoinRelation.NONE);
            }
            return _processorResult;
        }

        /// <summary>
        /// 根据表达式构建出相应结果
        /// </summary>
        /// <param name="expression"></param>
        /// <param name="joinRelation"></param>
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
                            _processorResult.Append(PredicateType.AND.ToString());
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
                        _processorResult.Append(PredicateType.OR.ToString());
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
                        _processorResult.Append(new MapperParameter(_parameterNameStack.Pop(), binaryExp.Value));
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
                        if (memberExp.Expression.NodeType == ExpressionType.Check)
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
                                if (!_aliasMapper.Any(a => a.Key == parameterExp.Type.GetEntityBaseAliasName().TableName && a.Value == parameterExp.Type.GetEntityBaseAliasName().AliasName))
                                {
                                    throw new ArgumentException($@"没有找到{parameterExp.Type.Name}所对应的形参");
                                }
                                internalAliasName = $@"{_aliasMapper.Where(w => w.Key == parameterExp.Type.GetEntityBaseAliasName().TableName && w.Value == parameterExp.Type.GetEntityBaseAliasName().AliasName).FirstOrDefault().Value.ToLower()}.";

                                var newParameterName = Guid.NewGuid().ToString().Replace("-", "");
                                _processorResult.Append(_template.CreatePredicate(_predicateTypeStack.Pop(), $@"{internalAliasName}{memberExp.Member.Name}", $"@{newParameterName}"));
                                _parameterNameStack.Push(newParameterName);
                            }
                        }
                        else
                        {
                            var getter = Expression.Lambda(memberExp).Compile();
                            _processorResult.Append(new MapperParameter(_parameterNameStack.Pop(), getter.DynamicInvoke()));
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
            Check.IfNullOrZero(expression);
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
            Check.IfNullOrZero(binary);
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
            Check.IfNullOrZero(binary);

            var leftMember = (MemberExpression)binary.Left;
            var rightMember = (MemberExpression)binary.Right;
            //表达式左右两边都不为常量时例如 xx.Id==yy.Id
            if (binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant)
            {
                var leftAliasName = ExtractMember(leftMember);
                var rightAliasName = ExtractMember(rightMember);

                var relationTemplate = _template.CreatePredicate(predicateType, $"{rightAliasName}.{rightMember.Member.Name}", $"{leftAliasName}.{leftMember.Member.Name}");
                _processorResult.Append(relationTemplate);
            }
            else if (binary.Left.NodeType == ExpressionType.Constant) //表达式左边为常量
            {
                var rightAliasName = ExtractMember(rightMember);
                var constant = (ConstantExpression)binary.Left;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _processorResult.Append(_template.CreatePredicate(predicateType, value + "", $"{rightAliasName}.{rightMember.Member.Name}"));
            }
            else if (binary.Right.NodeType == ExpressionType.Constant) //表达式的右边为常量
            {
                var leftAliasName = ExtractMember(leftMember);
                var constant = (ConstantExpression)binary.Right;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _processorResult.Append(_template.CreatePredicate(predicateType, $"{leftAliasName}.{leftMember.Member.Name}", value + ""));
            }
        }

        /// <summary>
        /// 提取表达式参数的类型别名
        /// </summary>
        /// <param name="memberExpression"></param>
        /// <returns></returns>
        private String ExtractMember(MemberExpression memberExpression)
        {
            Check.IfNullOrZero(memberExpression);
            var parameterExpression = (ParameterExpression)memberExpression.Expression;
            var (tableName, aliasName) = parameterExpression.Type.GetEntityBaseAliasName();
            if (!_aliasMapper.Any(a => a.Key == tableName && a.Value == aliasName))
            {
                throw new Exception($@"没有找到参数名:{memberExpression.Type.Name}所对应的表别名");
            }
            return _aliasMapper.Where(w => w.Key == tableName && w.Value == aliasName).FirstOrDefault().Value.ToLower();
        }

    }
}
