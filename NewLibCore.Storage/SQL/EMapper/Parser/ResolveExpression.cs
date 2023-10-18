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
    public class ResolveExpression
    {
        private readonly Stack<EMType> _emTypeStack;
        private readonly Stack<string> _placeHolderStack;
        private readonly EntityMapperOptions _options;
        private static int _parameterIndex = -1;

        internal List<MapperParameter> MapperParameters { get; private set; }

        internal StringBuilder TranslationResult { get; private set; } = new StringBuilder();

        internal ResolveExpression(IOptions<EntityMapperOptions> options)
        {
            Check.IfNullOrZero(options);

            _options = options.Value;
            _placeHolderStack = new Stack<string>();
            _emTypeStack = new Stack<EMType>();

            MapperParameters = new List<MapperParameter>();
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
                            TranslationResult.Append(EMType.AND.ToString());
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
                        TranslationResult.Append(EMType.OR.ToString());
                        Translate(binaryExp.Right, joinRelation);
                        break;
                    }
                case ExpressionType.Constant:
                case ExpressionType.Equal:
                case ExpressionType.GreaterThan:
                case ExpressionType.NotEqual:
                case ExpressionType.GreaterThanOrEqual:
                case ExpressionType.LessThan:
                case ExpressionType.LessThanOrEqual:
                    {
                        var binaryExp = (BinaryExpression)expression;
                        ResolvingBinaryExpression(binaryExp, Enum.Parse<EMType>(expression.NodeType.ToString(), true), joinRelation);
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
                                var placeHolder = $@"p{(++_parameterIndex)}";

                                TranslationResult.Append(_options.TemplateBase.CreatePredicate(_emTypeStack.Pop(), $@"{entityBaseAliasName.AliasName}.{memberExp.Member.Name}", $"@{placeHolder}"));
                                _placeHolderStack.Push(placeHolder);
                            }
                        }
                        else
                        {
                            var getter = Expression.Lambda(memberExp).Compile();
                            MapperParameters.Add(new MapperParameter(_placeHolderStack.Pop(), getter.DynamicInvoke()));
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

        /// <summary>
        /// 创建谓词语句
        /// </summary>
        private void ResolvingBinaryExpression(BinaryExpression binary, EMType predicateType, EMType joinRelation)
        {
            Check.IfNullOrZero(binary);

            if (new[] { EMType.INNER, EMType.LEFT, EMType.RIGHT, EMType.SEIF, EMType.CROSS }.Contains(joinRelation))
            {
                var leftMember = (MemberExpression)binary.Left;
                var rightMember = (MemberExpression)binary.Right;

                var leftAliasName = ((ParameterExpression)leftMember.Expression).Type.GetEntityBaseAliasName().AliasName;
                var rightAliasName = ((ParameterExpression)rightMember.Expression).Type.GetEntityBaseAliasName().AliasName;

                //表达式左右两边都不为常量时例如 xx.Id==yy.Id
                if (binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant)
                {
                    var relationTemplate = _options.TemplateBase.CreatePredicate(predicateType, $"{rightAliasName}.{rightMember.Member.Name}", $"{leftAliasName}.{leftMember.Member.Name}");
                    TranslationResult.Append(relationTemplate);
                }
                else if (binary.Left.NodeType == ExpressionType.Constant) //表达式左边为常量
                {
                    var constant = (ConstantExpression)binary.Left;
                    var value = bool.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                    var relationTemplate = _options.TemplateBase.CreatePredicate(predicateType, value + "", $"{rightAliasName}.{rightMember.Member.Name}");
                    TranslationResult.Append(relationTemplate);
                }
                else if (binary.Right.NodeType == ExpressionType.Constant) //表达式的右边为常量
                {
                    var constant = (ConstantExpression)binary.Right;
                    var value = bool.TryParse(constant.Value.ToString(), out var result) ? (result ? 1 : 0).ToString() : constant.Value;
                    var relationTemplate = _options.TemplateBase.CreatePredicate(predicateType, $"{leftAliasName}.{leftMember.Member.Name}", value + "");
                    TranslationResult.Append(relationTemplate);
                }
            }
            else
            {
                _emTypeStack.Push(predicateType);

                //当表达式的左边和右边同时不为常量或者只有左边为常量的话，则从左到右解析表达式，否则从右到左解析表达式
                if ((binary.Left.NodeType != ExpressionType.Constant && binary.Right.NodeType != ExpressionType.Constant) || binary.Left.NodeType != ExpressionType.Constant)
                {
                    Translate(binary.Left, joinRelation);
                    Translate(binary.Right, joinRelation);
                }
                else
                {
                    Translate(binary.Right, joinRelation);
                    Translate(binary.Left, joinRelation);
                }
            }
        }
    }
}
