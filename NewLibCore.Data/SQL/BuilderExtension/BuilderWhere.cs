﻿using NewLibCore.Data.SQL.InternalDataStore;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    public class BuilderWhere<TModel> : ITranslate where TModel : class, new()
    {
        internal DatabaseSyntaxBuilder _syntaxBuilder = new MysqlSyntaxBuilder();

        internal StringBuilder _builder = new StringBuilder();

        internal Stack<String> _parameterNameStack = new Stack<String>();

        internal Stack<RelationType> _operationalCharacterStack = new Stack<RelationType>();

        private JoinType _joinType;

        private readonly IDictionary<String, String> _expressionParameterNameToTableAliasNameMappers = new Dictionary<String, String>();

        internal IList<SqlParameterMapper> WhereParameters { get; private set; } = new List<SqlParameterMapper>();

        public void Translate(Expression expression, JoinType joinType = JoinType.None, Boolean alias = false)
        {
            _builder.Clear();
            if (joinType != JoinType.None)
            {
                var lamdbaExp = (LambdaExpression)expression;
                var aliasName = GetLeftTableAliasName(lamdbaExp.Parameters[0]);
                InitExpressionParameterMapper(lamdbaExp.Parameters);
                _builder.Append($@"{joinType.GetDescription()} {aliasName}");
                if (alias)
                {
                    _builder.Append($@" AS {aliasName.ToLower()} ON ");
                }
                _joinType = joinType;
            }
            else
            {
                _builder.Append(" WHERE ");
            }

            InternalTranslate(expression);
        }

        private void InitExpressionParameterMapper(IList<ParameterExpression> parameters)
        {
            foreach (var item in parameters)
            {
                _expressionParameterNameToTableAliasNameMappers.Add(item.Name, item.Type.Name.ToLower());
            }
        }

        private String GetLeftTableAliasName(ParameterExpression parameterExpression)
        {
            return parameterExpression.Type.Name;
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

                    if (_joinType != JoinType.None)
                    {
                        GetJoin(binaryExp, RelationType.EQ);
                    }
                    else
                    {
                        _operationalCharacterStack.Push(RelationType.EQ);
                        InternalBuildWhere(binaryExp.Left);
                        InternalBuildWhere(binaryExp.Right);
                    }

                    break;
                }
                case ExpressionType.GreaterThan:
                {
                    var binaryExp = (BinaryExpression)expression;
                    if (_joinType != JoinType.None)
                    {
                        GetJoin(binaryExp, RelationType.GT);
                    }
                    _operationalCharacterStack.Push(RelationType.GT);
                    InternalBuildWhere(binaryExp.Left);
                    InternalBuildWhere(binaryExp.Right);
                    break;
                }
                case ExpressionType.NotEqual:
                {
                    var binaryExp = (BinaryExpression)expression;

                    if (_joinType != JoinType.None)
                    {
                        GetJoin(binaryExp, RelationType.NQ);
                    }
                    else
                    {
                        _operationalCharacterStack.Push(RelationType.NQ);
                        InternalBuildWhere(binaryExp.Left);
                        InternalBuildWhere(binaryExp.Right);
                    }

                    break;
                }
                case ExpressionType.GreaterThanOrEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    if (_joinType != JoinType.None)
                    {
                        GetJoin(binaryExp, RelationType.GE);
                    }
                    else
                    {
                        _operationalCharacterStack.Push(RelationType.GE);
                        InternalBuildWhere(binaryExp.Left);
                        InternalBuildWhere(binaryExp.Right);
                    }
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

                    if (_joinType != JoinType.None)
                    {
                        GetJoin(binaryExp, RelationType.LT);
                    }
                    else
                    {
                        _operationalCharacterStack.Push(RelationType.LT);
                        InternalBuildWhere(binaryExp.Left);
                        InternalBuildWhere(binaryExp.Right);
                    }
                    break;
                }
                case ExpressionType.LessThanOrEqual:
                {
                    var binaryExp = (BinaryExpression)expression;
                    if (_joinType != JoinType.None)
                    {
                        GetJoin(binaryExp, RelationType.LE);
                    }
                    else
                    {
                        _operationalCharacterStack.Push(RelationType.LE);
                        InternalBuildWhere(binaryExp.Left);
                        InternalBuildWhere(binaryExp.Right);
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
                    var parameterExp = (ParameterExpression)memberExpression.Expression;
                    var memberName = memberExpression.Member.Name;
                    //var left = Expression.Parameter(typeof(TModel), ((ParameterExpression)memberExpression.Expression).Name);
                    var newMember = Expression.MakeMemberAccess(parameterExp, parameterExp.Type.GetMember(memberName)[0]);
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
                    _builder.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {(result ? 1 : 0)} ");
                }
                else
                {
                    _builder.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {constant.Value} ");
                }
            }
            else
            {
                var rightMemberExp = (MemberExpression)binaryExp.Right;
                var rightAliasName = _expressionParameterNameToTableAliasNameMappers[((ParameterExpression)rightMemberExp.Expression).Name];

                _builder.Append($@" {leftAliasName}.{leftMemberExp.Member.Name} {relationType.GetDescription()} {rightAliasName}.{rightMemberExp.Member.Name} ");
            }
        }
    }
}
