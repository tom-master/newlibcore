using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 谓词表达式翻译
    /// </summary>
    public class ExpressionTranslator
    {
        private readonly Stack<EMType> _emTypeStack;
        private readonly Stack<string> _parameterNameStack;
        private readonly EntityMapperOptions _options;
        private static int _parameterIndex = -1;
        internal List<MapperParameter> MapperParameters { get; private set; }

        internal List<KeyValuePair<string, string>> AliasMapper { get; set; }

        internal ExpressionTranslator(IOptions<EntityMapperOptions> options)
        {
            Check.IfNullOrZero(options);

            _options = options.Value;
            _parameterNameStack = new Stack<string>();
            _emTypeStack = new Stack<EMType>();

            AliasMapper = new List<KeyValuePair<string, string>>();
            MapperParameters = new List<MapperParameter>();
        }

        internal StringBuilder TranslationResult { get; private set; } = new StringBuilder();
        private void AppendResult(string value)
        {
            if (string.IsNullOrEmpty(value))
            {
                return;
            }
            TranslationResult.Append(value);
        }

        /// <summary>
        /// 根据表达式构建出相应结果
        /// </summary>
        /// <param name="expression"></param>
        /// <param name="joinRelation"></param>
        internal void Translate(Expression expression, EMType joinRelation)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                    {
                        var binaryExp = (BinaryExpression)expression;

                        if (binaryExp.Left.NodeType != ExpressionType.Constant && binaryExp.Right.NodeType != ExpressionType.Constant)
                        {
                            Translate(binaryExp.Left, joinRelation);
                            AppendResult(EMType.AND.ToString());
                            Translate(binaryExp.Right, joinRelation);
                        }
                        else
                        {
                            if (binaryExp.Left.NodeType != ExpressionType.Constant)
                            {
                                Translate(binaryExp.Left, joinRelation);
                            }
                            else if (binaryExp.Right.NodeType != ExpressionType.Constant)
                            {
                                Translate(binaryExp.Right, joinRelation);
                            }
                        }

                        break;
                    }
                case ExpressionType.OrElse:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        Translate(binaryExp.Left, joinRelation);
                        AppendResult(EMType.OR.ToString());
                        Translate(binaryExp.Right, joinRelation);
                        break;
                    }
                case ExpressionType.Call:
                    {
                        ProcessMethodCall(expression, joinRelation);
                        break;
                    }
                case ExpressionType.Constant:
                    {
                        var binaryExp = (ConstantExpression)expression;
                        MapperParameters.Add(new MapperParameter(_parameterNameStack.Pop(), binaryExp.Value));
                        break;
                    }
                case ExpressionType.Equal:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, EMType.EQ, joinRelation);
                        break;
                    }
                case ExpressionType.GreaterThan:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, EMType.GT, joinRelation);
                        break;
                    }
                case ExpressionType.NotEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, EMType.NQ, joinRelation);
                        break;
                    }
                case ExpressionType.GreaterThanOrEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, EMType.GE, joinRelation);
                        break;
                    }
                case ExpressionType.LessThan:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, EMType.LT, joinRelation);
                        break;
                    }
                case ExpressionType.LessThanOrEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        CreatePredicate(binaryExp, EMType.LE, joinRelation);
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
                            Translate(expression4, joinRelation);
                        }
                        else if (lamdbaExp.Body is MemberExpression expression3)
                        {
                            Translate(expression3, joinRelation);
                        }
                        else if (lamdbaExp.Body is MethodCallExpression expression2)
                        {
                            Translate(expression2, joinRelation);
                        }
                        else if (lamdbaExp.Body is UnaryExpression expression1)
                        {
                            Translate(expression1, joinRelation);
                        }
                        break;
                    }
                case ExpressionType.MemberAccess:
                    {
                        var memberExp = (MemberExpression)expression;
                        if (memberExp.Expression.NodeType == ExpressionType.Parameter)
                        {
                            if (_emTypeStack.Count == 0)
                            {
                                if (memberExp.Type == typeof(bool))
                                {
                                    var parameterExp = (ParameterExpression)memberExp.Expression;
                                    var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberExp.Member.Name)[0]);
                                    var newExpression = Expression.Equal(newMember, Expression.Constant(true));
                                    Translate(newExpression, joinRelation);
                                }
                            }
                            else
                            {
                                var parameterExp = (ParameterExpression)memberExp.Expression;

                                var entityBaseAliasName = parameterExp.Type.GetEntityBaseAliasName();
                                if (!AliasMapper.Any(a => a.Key == entityBaseAliasName.TableName && a.Value == entityBaseAliasName.AliasName))
                                {
                                    throw new ArgumentException($@"没有找到{parameterExp.Type.Name}所对应的形参");
                                }
                                var internalAliasName = $@"{AliasMapper.Where(w => w.Key == entityBaseAliasName.TableName && w.Value == entityBaseAliasName.AliasName).FirstOrDefault().Value.ToLower()}.";

                                var parameterPlaceHolder = $@"p{(++_parameterIndex)}";
                                AppendResult(_options.TemplateBase.CreatePredicate(_emTypeStack.Pop(), $@"{internalAliasName}{memberExp.Member.Name}", $"@{parameterPlaceHolder}"));
                                _parameterNameStack.Push(parameterPlaceHolder);
                            }
                        }
                        else
                        {
                            var getter = Expression.Lambda(memberExp).Compile();
                            MapperParameters.Add(new MapperParameter(_parameterNameStack.Pop(), getter.DynamicInvoke()));
                        }
                        break;
                    }
                case ExpressionType.Not:
                    {
                        var memberExpression = (MemberExpression)((UnaryExpression)expression).Operand;
                        var parameterExp = (ParameterExpression)memberExpression.Expression;
                        var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberExpression.Member.Name)[0]);
                        Translate(Expression.NotEqual(newMember, Expression.Constant(true)), joinRelation);
                        break;
                    }
                case ExpressionType.Convert:
                    {
                        var exp = ((UnaryExpression)expression).Operand;
                        Translate(exp, joinRelation);
                        break;
                    }
                default:
                    {
                        throw new NotSupportedException($@"暂不支持的表达式操作:{expression.NodeType}");
                    }
            }
        }

        private void ProcessMethodCall(Expression expression, EMType joinRelation)
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

            EMType predicateType = default;
            if (methodName == "StartsWith")
            {
                predicateType = EMType.START_LIKE;
            }
            else if (methodName == "EndsWith")
            {
                predicateType = EMType.END_LIKE;
            }
            else if (methodName == "Contains")
            {
                if (argumentType == typeof(string))
                {
                    predicateType = EMType.FULL_LIKE;
                }
                else if (argumentType.IsCollection())
                {
                    predicateType = EMType.IN;
                }
            }
            else
            {
                throw new Exception("暂不支持的方法");
            }

            _emTypeStack.Push(predicateType);

            if (argumentType == typeof(string))
            {
                Translate(obj, joinRelation);
                Translate(argument, joinRelation);
            }
            else if (argumentType.IsCollection())
            {
                Translate(argument, joinRelation);
                Translate(obj, joinRelation);
            }
        }

        /// <summary>
        /// 创建谓词语句
        /// </summary>
        /// <param name="binary"></param>
        /// <param name="predicateType"></param>
        /// <param name="joinRelation"></param>
        private void CreatePredicate(BinaryExpression binary, EMType predicateType, EMType joinRelation)
        {
            Check.IfNullOrZero(binary);
            var binaryExp = binary;

            if (new[] { EMType.INNER, EMType.LEFT, EMType.RIGHT, EMType.SEIF, EMType.CROSS }.Contains(joinRelation))
            {
                GetJoin(binaryExp, predicateType, joinRelation);
            }
            else
            {
                _emTypeStack.Push(predicateType);

                //当表达式的左边和右边同时不为常量或者只有左边为常量的话，则从左到右解析表达式，否则从右到左解析表达式
                if ((binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant) || binary.Left.NodeType != ExpressionType.Constant)
                {
                    Translate(binaryExp.Left, joinRelation);
                    Translate(binaryExp.Right, joinRelation);
                }
                else
                {
                    Translate(binaryExp.Right, joinRelation);
                    Translate(binaryExp.Left, joinRelation);
                }
            }
        }

        /// <summary>
        /// 将表达式翻译为相应的连接条件
        /// </summary>
        /// <param name="binary">表达式</param>
        /// <param name="relationType">关系类型</param>
        private void GetJoin(BinaryExpression binary, EMType predicateType, EMType joinRelation)
        {
            Check.IfNullOrZero(binary);
            var leftMember = (MemberExpression)binary.Left;
            var rightMember = (MemberExpression)binary.Right;
            //表达式左右两边都不为常量时例如 xx.Id==yy.Id
            if (binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant)
            {
                var leftAliasName = ExtractMember(leftMember);
                var rightAliasName = ExtractMember(rightMember);

                var relationTemplate = _options.TemplateBase.CreatePredicate(predicateType, $"{rightAliasName}.{rightMember.Member.Name}", $"{leftAliasName}.{leftMember.Member.Name}");
                AppendResult(relationTemplate);
            }
            else if (binary.Left.NodeType == ExpressionType.Constant) //表达式左边为常量
            {
                var rightAliasName = ExtractMember(rightMember);
                var constant = (ConstantExpression)binary.Left;
                var value = bool.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                var relationTemplate = _options.TemplateBase.CreatePredicate(predicateType, value + "", $"{rightAliasName}.{rightMember.Member.Name}");
                AppendResult(relationTemplate);
            }
            else if (binary.Right.NodeType == ExpressionType.Constant) //表达式的右边为常量
            {
                var leftAliasName = ExtractMember(leftMember);
                var constant = (ConstantExpression)binary.Right;
                var value = bool.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                var relationTemplate = _options.TemplateBase.CreatePredicate(predicateType, $"{leftAliasName}.{leftMember.Member.Name}", value + "");
                AppendResult(relationTemplate);
            }
        }

        /// <summary>
        /// 提取表达式参数的类型别名
        /// </summary>
        /// <param name="memberExpression"></param>
        /// <returns></returns>
        private string ExtractMember(MemberExpression memberExpression)
        {
            Check.IfNullOrZero(memberExpression);
            var parameterExpression = (ParameterExpression)memberExpression.Expression;
            var (tableName, aliasName) = parameterExpression.Type.GetEntityBaseAliasName();
            if (!AliasMapper.Any(a => a.Key == tableName && a.Value == aliasName))
            {
                throw new Exception($@"没有找到参数名:{memberExpression.Type.Name}所对应的表别名");
            }
            return AliasMapper.Where(w => w.Key == tableName && w.Value == aliasName).FirstOrDefault().Value.ToLower();
        }
    }
}
