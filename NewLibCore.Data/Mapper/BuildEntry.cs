using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;
using NewLibCore.Data.Mapper.InternalDataStore;

namespace NewLibCore.Data.Mapper
{
	internal class BuildEntry<TModel> where TModel : class, new()
	{
		private Stack<String> _parameterStack = new Stack<String>();

		private Stack<String> _operationalCharacterStack = new Stack<String>();

		private StringBuilder _builder = new StringBuilder();

		private IList<ParameterMapper> _parameters = new List<ParameterMapper>();

		private TModel _model;

		internal BuildEntry(TModel model)
		{
			_model = model;
		}

		internal void BuildWhere(Expression expression)
		{
			_builder.Append(" WHERE ");
			InternalBuildWhere(expression);
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
								_operationalCharacterStack.Push(RelationType.FIND_IN_SET.ToString());
								InternalBuildWhere(argument);
								InternalBuildWhere(obj);
							}

						}
						break;
					}
				case ExpressionType.Constant:
					{
						var binaryExp = (ConstantExpression)expression;
						_parameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", binaryExp.Value));
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
								_builder.Append(memberName, _operationalCharacterStack.Pop(), newParameterName);
								_parameterStack.Push(newParameterName);
							}
						}
						else
						{
							var getter = Expression.Lambda(memberExp).Compile();
							Object result = result = getter.DynamicInvoke();
							_parameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", result));
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

		internal void AppendSqlPart(String value)
		{
			_builder.Append(value);
		}

		internal void AppendParameter(IList<PropertyInfo> propertyInfos)
		{
			foreach (var item in propertyInfos.ToList().Select(c => new ParameterMapper($@"@{c.Name}", c.GetValue(_model))))
			{
				_parameters.Add(item);
			}
		}

		internal IList<ParameterMapper> GetParameters()
		{
			return _parameters;
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

		FIND_IN_SET = 4
	}

	public static class StringBuilderExtension
	{
		public static void Append(this StringBuilder builder, String left, String opt, String right)
		{
			if (opt.ToUpper() == "FIND_IN_SET")
			{
				builder.Append($@" FIND_IN_SET({left}, @{right})>0 ");
			}
			else if (opt.ToUpper() == "LIKE")
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
