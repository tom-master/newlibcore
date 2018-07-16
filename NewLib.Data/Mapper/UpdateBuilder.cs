using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Linq.Expressions;

namespace NewLib.Data.Mapper
{
	public class UpdateBuilder<TModel>: SqlBuilder<TModel> where TModel : class, new()
	{
		private Expression<Func<TModel, Boolean>> _where;


		public UpdateBuilder(TModel model, Expression<Func<TModel, Boolean>> where = null) : base(model)
		{
			if (model == null)
			{
				throw new ArgumentException($@"{nameof(model)} is null");
			}

			_where = where;
		}

		public override String ParseToSql()
		{
			ResetBuilder();
			ResetParameters();

			Append($@"UPDATE {ModelType.Name} SET ");
			Append($@"{String.Join(",", GetFields().Select(s => $@"{s}=@{s}"))}");

			if (_where != null)
			{
				GenerateCondition(_where);
			}

			Append($@" AND IsDeleted=0");
			Append($@" SELECT CAST(@@ROWCOUNT AS INT) ");
			return ToString();
		}

		protected override IEnumerable<string> GetFields()
		{
			var method = ModelType.GetMethod("GetPropertyValues").Invoke(ModelInstance, null);
			var returnValue = method as IDictionary<String, Object>;
			if (returnValue != null)
			{
				foreach (var item in returnValue)
				{
					Parameters.Add(new SqlParameter($@"@{item.Key}", item.Value));
					yield return item.Key;
				}
			}
		}

		protected override Object ParseValue(Object value)
		{
			return value;
		}

		private void AppendField(Object fieldName)
		{
			Append($@" {fieldName} ");
		}

		private void GenerateCondition(Expression expression)
		{
			if (!ToString().Contains("WHERE"))
			{
				Append(" WHERE ");
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
					GenerateCondition(binaryExp.Left);
					And();
					GenerateCondition(binaryExp.Right);
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
					break;
				case ExpressionType.Equal:
				{
					var binaryExp = (BinaryExpression)expression;
					GenerateCondition(binaryExp.Left);
					if (binaryExp.Right.NodeType != ExpressionType.Constant)
					{
						GenerateCondition(binaryExp.Right);
					}
					else
					{
						Parameters.Add(new SqlParameter($@"@{ParameterStack.Pop()}", ((ConstantExpression)binaryExp.Right).Value));
					}
					break;
				}
				case ExpressionType.GreaterThan:
					break;
				case ExpressionType.GreaterThanOrEqual:
					break;
				case ExpressionType.Invoke:
					break;
				case ExpressionType.Lambda:
				{
					var lamdbaExp = (LambdaExpression)expression;

					if (lamdbaExp.Body.NodeType == ExpressionType.MemberAccess)
					{
						GenerateCondition((MemberExpression)lamdbaExp.Body);
					}
					else
					{
						GenerateCondition((BinaryExpression)lamdbaExp.Body);
					}

					break;
				}
				case ExpressionType.LessThan:
					break;
				case ExpressionType.LessThanOrEqual:
					break;
				case ExpressionType.MemberAccess:
				{
					var memberExp = (MemberExpression)expression;
					if (memberExp.Expression.NodeType == ExpressionType.Parameter)
					{
						var memberName = memberExp.Member.Name;
						var newParameterName = $@"{memberName}{GetTimeStamp()}";
						AppendField($@"{memberName}=@{newParameterName}");
						ParameterStack.Push($@"{newParameterName}");
					}
					else
					{
						switch (memberExp.Type.Name.ToLower())
						{
							case "int32":
							{
								var getter = Expression.Lambda<Func<Int32>>(memberExp).Compile();
								Parameters.Add(new SqlParameter($@"@{ParameterStack.Pop()}", getter()));
								break;
							}
							case "string":
							{
								var getter = Expression.Lambda<Func<String>>(memberExp).Compile();
								Parameters.Add(new SqlParameter($@"@{ParameterStack.Pop()}", getter()));
								break;
							}
							default:
								break;
						}
					}
					break;
				}
				case ExpressionType.Not:
					break;
				case ExpressionType.NotEqual:
					break;
				case ExpressionType.Or:
					break;
				case ExpressionType.OrElse:
				{
					var binaryExp = (BinaryExpression)expression;
					GenerateCondition(binaryExp.Left);
					Or();
					GenerateCondition(binaryExp.Right);
					break;
				}
				case ExpressionType.Parameter:
				{
					var parameterExp = (ParameterExpression)expression;
					break;
				}
				case ExpressionType.IsTrue:
					break;
				case ExpressionType.IsFalse:
					break;
				default:
					break;
			}
		}

		/// <summary>  
		/// 获取时间戳  
		/// </summary>  
		/// <returns></returns>  
		private string GetTimeStamp()
		{
			TimeSpan ts = DateTime.UtcNow - new DateTime(1970, 1, 1, 0, 0, 0, 0);
			return Convert.ToInt64(ts.TotalSeconds).ToString();
		}
	}
}
