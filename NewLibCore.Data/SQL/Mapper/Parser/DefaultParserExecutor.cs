using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{

    internal class ParseModel
    {
        internal String Sql { get; set; }

        internal IEnumerable<MapperParameter> Parameters { get; set; }

        internal ExpressionStore ExpressionStore { get; set; }
    }

    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal abstract class ParserExecutor
    {

        private readonly ParserResult _parserResult;

        internal ParserExecutor(ParserResult parserResult)
        {
            _parserResult = parserResult;
        }


        /// <summary>
        /// 翻译
        /// </summary>
        /// <returns></returns>
        protected abstract ParserResult InnerParse(ParserResult parserResult, ExpressionStore expressionStore);

        internal ParserResult Parse(ParseModel parseModel)
        {

            _parserResult.Append(parseModel.Sql, parseModel.Parameters.ToArray());

            if (parseModel.ExpressionStore != null)
            {
                InnerParse(_parserResult, parseModel.ExpressionStore);
            }

            return _parserResult;
        }
    }

    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal class DefaultParserExecutor : ParserExecutor
    {
        private JoinRelation _joinRelation;

        private ParserResult _parserResult;

        private Stack<String> _parameterNameStack;

        private Stack<PredicateType> _predicateTypeStack;

        private IReadOnlyList<KeyValuePair<String, String>> _tableAliasMapper;

        private readonly TemplateBase _templateBase;

        /// <summary>
        /// 初始化Parser类的新实例
        /// </summary>
        /// <param name="templateBase"></param>
        public DefaultParserExecutor(TemplateBase templateBase, ParserResult parserResult) : base(parserResult)
        {
            Parameter.Validate(templateBase);

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
        protected override ParserResult InnerParse(ParserResult parserResult, ExpressionStore expressionStore)
        {
            Parameter.Validate(parserResult);
            Parameter.Validate(expressionStore);

            _parserResult = parserResult;

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
                    _parserResult.Append(_templateBase.CreateJoin(item.JoinRelation, aliasItem.Key, aliasItem.Value.ToLower()));

                    //设置相应的连接类型
                    _joinRelation = item.JoinRelation;

                    //获取连接类型中的存储的表达式对象进行翻译
                    InternalParser(item.Expression);
                }
            }
            _parserResult.Append(" WHERE 1=1 ");
            //翻译Where条件对象
            if (expressionStore.Where != null)
            {
                var lambdaExp = (LambdaExpression)expressionStore.Where.Expression;
                //当表达式主体为常量时则直接返回，不做解析
                if (lambdaExp.Body.NodeType == ExpressionType.Constant)
                {
                    return _parserResult;
                }

                _joinRelation = JoinRelation.NONE;
                _parserResult.Append(PredicateType.AND.ToString());

                //获取Where类型中的存储的表达式对象进行翻译
                InternalParser(lambdaExp);
            }
            return _parserResult;
        }

        /// <summary>
        /// 根据表达式构建出相应结果
        /// </summary>
        /// <param name="expression">表达式</param>
        private void InternalParser(Expression expression)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                    {
                        var binaryExp = (BinaryExpression)expression;

                        if (binaryExp.Left.NodeType != ExpressionType.Constant && binaryExp.Right.NodeType != ExpressionType.Constant)
                        {
                            InternalParser(binaryExp.Left);
                            _parserResult.Append(PredicateType.AND.ToString());
                            InternalParser(binaryExp.Right);
                        }
                        else
                        {
                            if (binaryExp.Left.NodeType != ExpressionType.Constant)
                            {
                                InternalParser(binaryExp.Left);
                            }
                            else if (binaryExp.Right.NodeType != ExpressionType.Constant)
                            {
                                InternalParser(binaryExp.Right);
                            }
                        }

                        break;
                    }
                case ExpressionType.OrElse:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        InternalParser(binaryExp.Left);
                        _parserResult.Append(PredicateType.OR.ToString());
                        InternalParser(binaryExp.Right);
                        break;
                    }
                case ExpressionType.Call:
                    {
                        ParserMethodCall(expression);
                        break;
                    }
                case ExpressionType.Constant:
                    {
                        var binaryExp = (ConstantExpression)expression;
                        _parserResult.Append(new MapperParameter(_parameterNameStack.Pop(), binaryExp.Value));
                        break;
                    }
                case ExpressionType.Equal:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.EQ);
                        break;
                    }
                case ExpressionType.GreaterThan:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.GT);
                        break;
                    }
                case ExpressionType.NotEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.NQ);
                        break;
                    }
                case ExpressionType.GreaterThanOrEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.GE);
                        break;
                    }
                case ExpressionType.LessThan:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.LT);
                        break;
                    }
                case ExpressionType.LessThanOrEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, PredicateType.LE);
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
                            InternalParser((BinaryExpression)lamdbaExp.Body);
                        }
                        else if (lamdbaExp.Body is MemberExpression)
                        {
                            InternalParser((MemberExpression)lamdbaExp.Body);
                        }
                        else if (lamdbaExp.Body is MethodCallExpression)
                        {
                            InternalParser((MethodCallExpression)lamdbaExp.Body);
                        }
                        else if (lamdbaExp.Body is UnaryExpression)
                        {
                            InternalParser((UnaryExpression)lamdbaExp.Body);
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
                                    InternalParser(newExpression);
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
                                _parserResult.Append(_templateBase.CreatePredicate(_predicateTypeStack.Pop(), $@"{internalAliasName}{memberExp.Member.Name}", $"@{newParameterName}"));
                                _parameterNameStack.Push(newParameterName);
                            }
                        }
                        else
                        {
                            var getter = Expression.Lambda(memberExp).Compile();
                            _parserResult.Append(new MapperParameter(_parameterNameStack.Pop(), getter.DynamicInvoke()));
                        }
                        break;
                    }
                case ExpressionType.Not:
                    {
                        var memberExpression = (MemberExpression)((UnaryExpression)expression).Operand;
                        var parameterExp = (ParameterExpression)memberExpression.Expression;
                        var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberExpression.Member.Name)[0]);
                        InternalParser(Expression.NotEqual(newMember, Expression.Constant(true)));
                        break;
                    }
                case ExpressionType.Convert:
                    {
                        var exp = ((UnaryExpression)expression).Operand;
                        InternalParser(exp);
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
        private void ParserMethodCall(Expression expression)
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
                else if (argumentType.IsCollections())
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
                InternalParser(obj);
                InternalParser(argument);
            }
            else if (argumentType.IsCollections())
            {
                InternalParser(argument);
                InternalParser(obj);
            }
        }

        /// <summary>
        /// 创建谓词语句
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <param name="relationType">关系类型</param>
        private void CreatePredicate(BinaryExpression binary, PredicateType predicateType)
        {
            var binaryExp = binary;
            if (_joinRelation != JoinRelation.NONE)
            {
                GetJoin(binaryExp, predicateType);
            }
            else
            {
                _predicateTypeStack.Push(predicateType);

                //当表达式的左边和右边同时不为常量或者只有左边为常量的话，则从左到右解析表达式，否则从右到左解析表达式
                if ((binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant) || binary.Left.NodeType != ExpressionType.Constant)
                {
                    InternalParser(binaryExp.Left);
                    InternalParser(binaryExp.Right);
                }
                else
                {
                    InternalParser(binaryExp.Right);
                    InternalParser(binaryExp.Left);
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
            Parameter.Validate(binary);

            //表达式左右两边都不为常量时例如 xx.Id==yy.Id
            if (binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant)
            {
                var (LeftMember, LeftAliasName) = ParserLeft(binary);
                var (RightMember, RightAliasName) = ParserRight(binary);

                var relationTemplate = _templateBase.CreatePredicate(predicateType, $"{RightAliasName}.{RightMember.Member.Name}", $"{LeftAliasName}.{LeftMember.Member.Name}");
                _parserResult.Append(relationTemplate);
            }
            else if (binary.Left.NodeType == ExpressionType.Constant) //表达式左边为常量
            {
                var (RightMember, RightAliasName) = ParserRight(binary);
                var constant = (ConstantExpression)binary.Left;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _parserResult.Append(_templateBase.CreatePredicate(predicateType, value + "", $"{RightAliasName}.{RightMember.Member.Name}"));
            }
            else if (binary.Right.NodeType == ExpressionType.Constant) //表达式的右边为常量
            {
                var (LeftMember, LeftAliasName) = ParserLeft(binary);
                var constant = (ConstantExpression)binary.Right;
                var value = Boolean.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                _parserResult.Append(_templateBase.CreatePredicate(predicateType, $"{LeftAliasName}.{LeftMember.Member.Name}", value + ""));
            }
        }

        /// <summary>
        /// 获取右表达式的成员对象和别名
        /// </summary>
        /// <param name="binaryExp">表达式</param>
        /// <returns></returns>
        private (MemberExpression RightMember, String RightAliasName) ParserRight(BinaryExpression binaryExp)
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
        /// 获取左表达式的成员对象和别名
        /// </summary>
        /// <param name="binaryExp">表达式</param>
        /// <returns></returns>
        private (MemberExpression LeftMember, String LeftAliasName) ParserLeft(BinaryExpression binaryExp)
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
