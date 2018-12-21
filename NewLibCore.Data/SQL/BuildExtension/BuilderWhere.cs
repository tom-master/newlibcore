using NewLibCore.Data.SQL.InternalDataStore;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal class BuilderWhere<TModel> : ITranslate where TModel : class, new()
    {
        internal DatabaseSyntaxBuilder _syntaxBuilder = new MysqlSyntaxBuilder(true);

        internal StringBuilder _builder = new StringBuilder();

        internal Stack<String> _parameterNameStack = new Stack<String>();

        internal Stack<RelationType> _operationalCharacterStack = new Stack<RelationType>();

        internal IList<SqlParameterMapper> WhereParameters { get; private set; } = new List<SqlParameterMapper>();

        public void Translate(Expression expression)
        {
            _builder.Append(" WHERE ");
            InternalTranslate(expression);
        }

        private void InternalTranslate(Expression expression)
        {
            InternalBuildWhere(expression);
        }

        public override String ToString()
        {
            return _builder.ToString();
        }

        private void InternalBuildWhere(Expression expression)
        {
            switch (expression.NodeType)
            {
                case ExpressionType.AndAlso:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    _builder.Append(RelationType.AND.ToString());
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.Call:
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
                    break;
                }
                case ExpressionType.Constant:
                {
                    var binaryExp = (ConstantExpression)expression;
                    WhereParameters.Add(new SqlParameterMapper($@"@{_parameterNameStack.Pop()}", binaryExp.Value));
                    break;
                }
                case ExpressionType.Equal:
                {
                    var binaryExp = (BinaryExpression)expression;
                    _operationalCharacterStack.Push(RelationType.EQ);
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.GreaterThan:
                {
                    var binaryExp = (BinaryExpression)expression;
                    _operationalCharacterStack.Push(RelationType.GT);
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.NotEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    _operationalCharacterStack.Push(RelationType.NQ);
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.GreaterThanOrEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    _operationalCharacterStack.Push(RelationType.GE);
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
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
                case ExpressionType.LessThan:
                {
                    var binaryExp = (BinaryExpression)expression;
                    _operationalCharacterStack.Push(RelationType.LT);
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.LessThanOrEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    _operationalCharacterStack.Push(RelationType.LE);
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
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
                                var left = Expression.Parameter(typeof(TModel), ((ParameterExpression)memberExp.Expression).Name);
                                var newMember = Expression.MakeMemberAccess(left, left.Type.GetMember(memberName)[0]);
                                var newExpression = Expression.Equal(newMember, Expression.Constant(true));
                                InternalBuildWhere(newExpression);
                            }
                        }
                        else
                        {
                            var syntax = _syntaxBuilder.SyntaxBuilder(_operationalCharacterStack.Pop(), memberName, newParameterName);
                            _builder.Append(syntax);
                            _parameterNameStack.Push(newParameterName);
                        }
                    }
                    else
                    {
                        var getter = Expression.Lambda(memberExp).Compile();
                        Object result = result = getter.DynamicInvoke();
                        WhereParameters.Add(new SqlParameterMapper($@"@{_parameterNameStack.Pop()}", result));
                        break;
                    }
                    break;
                }
                case ExpressionType.Not:
                {
                    var unaryExpression = (UnaryExpression)expression;
                    var memberExpression = (MemberExpression)unaryExpression.Operand;
                    var memberName = memberExpression.Member.Name;
                    var left = Expression.Parameter(typeof(TModel), ((ParameterExpression)memberExpression.Expression).Name);
                    var newMember = Expression.MakeMemberAccess(left, left.Type.GetMember(memberName)[0]);
                    var newExpression = Expression.NotEqual(newMember, Expression.Constant(true));
                    InternalBuildWhere(newExpression);
                    break;
                }
                case ExpressionType.OrElse:
                {
                    var binaryExp = (BinaryExpression)expression;
                    InternalBuildWhere(binaryExp.Left);
                    _builder.Append(RelationType.OR.ToString());
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                default:
                    break;
            }
        }

    }
}
