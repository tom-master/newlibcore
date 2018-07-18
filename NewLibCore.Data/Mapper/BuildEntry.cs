using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq.Expressions;
using System.Text;
using NewLibCore.Data.Mapper.InternalDataStore;

namespace NewLibCore.Data.Mapper
{
	internal class BuildEntry<TModel> where TModel : class, new()
	{
		private Stack<String> _parameterStack = new Stack<String>();
		private Stack<String> _operationalCharacterStack = new Stack<String>();
		private StringBuilder _builder = new StringBuilder();
		private RelationType _temp;

		internal void BuildWhere(Expression expression)
		{
			if (!_builder.ToString().Trim().Contains("WHERE"))
			{
				_builder.Append(" WHERE ");
			}

			switch (expression.NodeType)
			{
				case ExpressionType.AndAlso:
					{
						var binaryExp = (BinaryExpression)expression;
						BuildWhere(binaryExp.Left);
						_builder.Append(RelationType.AND.ToString());
						BuildWhere(binaryExp.Right);
						break;
					}
				case ExpressionType.Call:
					{
						var methodCallExp = (MethodCallExpression)expression;
						if (methodCallExp.Method.Name == "Contains")
						{
							_temp = RelationType.LIKE;
							_operationalCharacterStack.Push(RelationType.LIKE.ToString());
							BuildWhere(methodCallExp.Object);
							BuildWhere(methodCallExp.Arguments[0]);
						}
						break;
					}
				case ExpressionType.Constant:
					{
						var binaryExp = (ConstantExpression)expression;
						Parameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", binaryExp.Value));
					}
					break;
				case ExpressionType.Equal:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" = ");
						BuildWhere(binaryExp.Left);
						BuildWhere(binaryExp.Right);
						break;
					}
				case ExpressionType.GreaterThan:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" > ");
						BuildWhere(binaryExp.Left);
						BuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.NotEqual:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" <> ");
						BuildWhere(binaryExp.Left);
						BuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.GreaterThanOrEqual:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" >= ");
						BuildWhere(binaryExp.Left);
						BuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.Lambda:
					{
						var lamdbaExp = (LambdaExpression)expression;
						if (lamdbaExp.Body is BinaryExpression)
						{
							BuildWhere((BinaryExpression)lamdbaExp.Body);
						}
						else if (lamdbaExp.Body is MemberExpression)
						{
							BuildWhere((MemberExpression)lamdbaExp.Body);
						}
						else if (lamdbaExp.Body is MethodCallExpression)
						{
							BuildWhere((MethodCallExpression)lamdbaExp.Body);
						}
						else
						{
							BuildWhere((UnaryExpression)lamdbaExp.Body);
						}
						break;
					}
				case ExpressionType.LessThan:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" < ");
						BuildWhere(binaryExp.Left);
						BuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.LessThanOrEqual:
					{
						var binaryExp = (BinaryExpression)expression;
						_operationalCharacterStack.Push(" <= ");
						BuildWhere(binaryExp.Left);
						BuildWhere(binaryExp.Right);
					}
					break;
				case ExpressionType.MemberAccess:
					{
						var memberExp = (MemberExpression)expression;
						if (memberExp.Expression.NodeType == ExpressionType.Parameter)
						{
							var memberName = memberExp.Member.Name;
							var newParameterName = $@"{memberName}{GetTimeStamp()}";
							if (_operationalCharacterStack.Count == 0)
							{
								if (memberExp.Type == typeof(Boolean))
								{
									var left = Expression.Parameter(typeof(TModel), ((ParameterExpression)memberExp.Expression).Name);
									var newMember = Expression.MakeMemberAccess(left, left.Type.GetMember(memberName)[0]);
									var newExpression = Expression.Equal(newMember, Expression.Constant(true));
									BuildWhere(newExpression);
								}
							}
							else
							{
								_builder.Append($@" {memberName} {_operationalCharacterStack.Pop()} @{newParameterName} ");
								_parameterStack.Push(newParameterName);
							}
						}
						else
						{
							var getter = Expression.Lambda<Func<Object>>(memberExp).Compile();
							Object result = "";
							if (_temp == RelationType.LIKE)
							{
								result = $@"%{getter()}%";
							}
							else
							{
								result = getter();
							}
							Parameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", result));
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
						BuildWhere(newExpression);
						break;
					}
				case ExpressionType.OrElse:
					{
						var binaryExp = (BinaryExpression)expression;
						BuildWhere(binaryExp.Left);
						_builder.Append(RelationType.OR.ToString());
						BuildWhere(binaryExp.Right);
						break;
					}
				default:
					break;
			}
		}

		internal void Append(String value)
		{
			_builder.Append(value);
		}

		internal void ResetBuilder()
		{
			_builder.Clear();
		}

		internal IList<ParameterMapper> Parameters { get; } = new List<ParameterMapper>();

		private string GetTimeStamp()
		{
			return Guid.NewGuid().ToString().Replace("-", "");
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

		LIKE = 3
	}
}
