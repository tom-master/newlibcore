using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq.Expressions;
using System.Text;
using NewLibCore.Data.Mapper.InternalDataStore;

namespace NewLibCore.Data.Mapper
{
	internal class BuilderExtension<TModel> where TModel : new()
	{
		private Stack<String> _parameterStack = new Stack<String>();
		private Stack<String> _operationalCharacterStack = new Stack<String>();
		private IList<ParameterMapper> _parameters = new List<ParameterMapper>();
		private StringBuilder _builder = new StringBuilder();

		internal void BuildWhere(Expression expression)
		{
			if (!_builder.ToString().Trim().Contains("WHERE"))
			{
				_builder.Append(" WHERE ");
			}

			switch (expression.NodeType)
			{
				case ExpressionType.Add:
					break;
				case ExpressionType.AddChecked:
					break;
				case ExpressionType.And:
					break;
				case ExpressionType.AndAlso:
				{
					var binaryExp = (BinaryExpression)expression;
					BuildWhere(binaryExp.Left);
					_builder.Append(" AND ");
					BuildWhere(binaryExp.Right);
					break;
				}
				case ExpressionType.ArrayLength:
					break;
				case ExpressionType.ArrayIndex:
					break;
				case ExpressionType.Call:
					break;
				case ExpressionType.Conditional:
					break;
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
				case ExpressionType.Invoke:
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
							_builder.Append($@"{memberName}{_operationalCharacterStack.Pop()}@{newParameterName}");
							_parameterStack.Push(newParameterName);
						}

					}
					else
					{
						switch (memberExp.Type.Name.ToLower())
						{
							case "int32":
							{
								var getter = Expression.Lambda<Func<Int32>>(memberExp).Compile();
								_parameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", getter()));
								break;
							}
							case "string":
							{
								var getter = Expression.Lambda<Func<String>>(memberExp).Compile();
								_parameters.Add(new ParameterMapper($@"@{_parameterStack.Pop()}", getter()));
								break;
							}
							default:
								break;
						}
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
				case ExpressionType.Or:
					break;
				case ExpressionType.OrElse:
				{
					var binaryExp = (BinaryExpression)expression;
					BuildWhere(binaryExp.Left);
					_builder.Append(" OR ");
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

		internal BuildEntry GetBuildEntry()
		{
			return new BuildEntry(_builder, _parameters);
		}

		internal void ResetBuilder()
		{
			_builder.Clear();
		}

		internal void ResetParameters()
		{
			_parameters.Clear();
		}

		private string GetTimeStamp()
		{
			TimeSpan ts = DateTime.UtcNow - new DateTime(1970, 1, 1, 0, 0, 0, 0);
			return Convert.ToInt64(ts.TotalSeconds).ToString();
		} 
	}

	internal class BuildEntry
	{
		private StringBuilder _builder;
		private IList<ParameterMapper> _parameters;

		internal BuildEntry(StringBuilder builder, IList<ParameterMapper> parameters)
		{
			_builder = builder;
			_parameters = parameters;
		}

		internal String Sql { get { return _builder.ToString(); } }

		internal IList<ParameterMapper> Parameters { get { return _parameters; } }
	}
}
