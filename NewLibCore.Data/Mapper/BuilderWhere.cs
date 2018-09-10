using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;
using NewLibCore.Data.Mapper.InternalDataStore;

namespace NewLibCore.Data.Mapper
{
	internal class BuilderWhere<TModel> where TModel : class, new()
	{
		private StringBuilder _builder = new StringBuilder();

		private Stack<String> _parameterStack = new Stack<String>();

		private Stack<String> _operationalCharacterStack = new Stack<String>();

		private String _aliasName = "";

		internal BuilderWhere(String aliasName = "")
		{
			_aliasName = aliasName == "" ? "" : aliasName + ".";
		}

		internal void Where(Expression expression)
		{
			_builder.Append(" WHERE ");
			InternalBuildWhere(expression);
		}

		internal IList<ParameterMapper> WhereParameters { get; private set; } = new List<ParameterMapper>();

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
						if (methodCallExp.Method.Name == "Contains")
						{
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

							if (argumentType == typeof(String))
							{
								_operationalCharacterStack.Push(RelationType.LIKE.ToString());
								InternalBuildWhere(obj);
								InternalBuildWhere(argument);
							}
							else if (argumentType == typeof(Int32[]) || (argumentType.Name == "List`1" || argumentType.Name == "IList`1"))
							{
								_operationalCharacterStack.Push(RelationType.IN.ToString());
								InternalBuildWhere(argument);
								InternalBuildWhere(obj);
							}
						}
						break;
					}
				case ExpressionType.Constant:
					{
						var binaryExp = (ConstantExpression)expression;
						WhereParameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", binaryExp.Value));
					}
					break;
				case ExpressionType.Equal:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" = ");
						InternalBuildWhere(binaryExp.Left);
						InternalBuildWhere(binaryExp.Right);
						break;
					}
				case ExpressionType.GreaterThan:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" > ");
						InternalBuildWhere(binaryExp.Left);
						InternalBuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.NotEqual:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" <> ");
						InternalBuildWhere(binaryExp.Left);
						InternalBuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.GreaterThanOrEqual:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" >= ");
						InternalBuildWhere(binaryExp.Left);
						InternalBuildWhere(binaryExp.Right);
					}
					break;
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
						_operationalCharacterStack.Push(" < ");
						InternalBuildWhere(binaryExp.Left);
						InternalBuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.LessThanOrEqual:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" <= ");
						InternalBuildWhere(binaryExp.Left);
						InternalBuildWhere(binaryExp.Right);
					}
					break;
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
								_builder.Append($@"{_aliasName}{memberName}", _operationalCharacterStack.Pop(), newParameterName);
								_parameterStack.Push(newParameterName);
							}
						}
						else
						{
							var getter = Expression.Lambda(memberExp).Compile();
							Object result = result = getter.DynamicInvoke();
							WhereParameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", result));
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

		public override string ToString()
		{
			return _builder.ToString();
		}
	}

	internal enum RelationType
	{
		AND = 1,

		OR = 2,

		LIKE = 3,

		IN = 4
	}

	public static class StringBuilderExtension
	{
		public static void Append(this StringBuilder builder, String left, String opt, String right)
		{
			if (opt.ToUpper() == RelationType.IN.ToString())
			{
				builder.Append($@" FIND_IN_SET({left}, @{right})>0 ");
			}
			else if (opt.ToUpper() == RelationType.LIKE.ToString())
			{
				builder.Append($@" {left} {opt} CONCAT('%',@{right},'%') ");
			}
			else
			{
				builder.Append($@" {left} {opt} @{right} ");
			}
		}
	}
}
