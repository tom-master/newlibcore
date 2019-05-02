using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
	public interface IAddEntityMapper<TModel> where TModel : EntityBase, new()
	{
		TModel Add(TModel model);
	}

	public interface IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
	{
		Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression);
	}

	public interface ISelectEntityMapper<TModel> where TModel : EntityBase, new()
	{
		TModel ToOne();

		IList<TModel> ToList();

		Int32 Count(Expression<Func<TModel, Boolean>> where = null);

		ISelectEntityMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null);

		ISelectEntityMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression = null);

		ISelectEntityMapper<TModel> Page(Int32 pageIndex, Int32 pageSize, out Int32 totalCount);

		ISelectEntityMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

		ISelectEntityMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

		ISelectEntityMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

		ISelectEntityMapper<TModel> OrderBy<TOrder, TKey>(Expression<Func<TOrder, TKey>> order, OrderByType orderBy = OrderByType.DESC) where TOrder : EntityBase, new();
	}

	public class SelectEntityMapper<TModel> : ISelectEntityMapper<TModel> where TModel : EntityBase, new()
	{
		private readonly TModel _instance;

		public SelectEntityMapper()
		{
			_instance = Activator.CreateInstance<TModel>();
		}

		public TModel ToOne()
		{
			return ToList().FirstOrDefault();
		}

		public IList<TModel> ToList()
		{
			IBuilder<TModel> builder = new SelectBuilder<TModel>(_instance.StatementStore);
			var translationResult = builder.Build();
			var executeResult = _instance.ExecuteCore.Execute(ExecuteType.SELECT, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
			var dataTable = executeResult.Value as DataTable;
			return dataTable.AsList<TModel>();
		}

		public Int32 Count(Expression<Func<TModel, Boolean>> where = null)
		{
			if (where != null)
			{
				_instance.StatementStore.Add(where);
			}
			return default;
		}

		public ISelectEntityMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
		{
			if (fields != null)
			{
				_instance.StatementStore.Add(fields);
			}
			return default;
		}

		public ISelectEntityMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression = null)
		{
			if (expression != null)
			{
				_instance.StatementStore.Add(expression);
			}
			return this;
		}

		public ISelectEntityMapper<TModel> Page(Int32 pageIndex, Int32 pageSize, out Int32 totalCount)
		{
			totalCount = 0;
			return this;
		}

		public ISelectEntityMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
		{
			Parameter.Validate(expression);
			_instance.StatementStore.Add(expression, JoinType.LEFT);
			return this;
		}

		public ISelectEntityMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
		{
			Parameter.Validate(expression);
			_instance.StatementStore.Add(expression, JoinType.RIGHT);
			return this;
		}

		public ISelectEntityMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
		{
			Parameter.Validate(expression);
			_instance.StatementStore.Add(expression, JoinType.INNER);
			return this;
		}

		public ISelectEntityMapper<TModel> OrderBy<TOrder, TKey>(Expression<Func<TOrder, TKey>> order, OrderByType orderBy = OrderByType.DESC) where TOrder : EntityBase, new()
		{
			Parameter.Validate(order);
			_instance.StatementStore.AddOrderBy(order, orderBy);
			return this;
		}
	}

	public class UpdateEntityMapper<TModel> : IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
	{
		public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
		{
			model.StatementStore.Add(expression);

			IBuilder<TModel> builder = new ModifyBuilder<TModel>(model, model.StatementStore, true);
			var translationResult = builder.Build();
			var executeResult = model.ExecuteCore.Execute(ExecuteType.UPDATE, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
			return (Int32)executeResult.Value > 0;
		}
	}

	public class AddEntityMapper<TModel> : IAddEntityMapper<TModel> where TModel : EntityBase, new()
	{
		private readonly ExecuteCore _executeCore;

		public AddEntityMapper()
		{
			_executeCore = new ExecuteCore();
		}

		public TModel Add(TModel model)
		{
			IBuilder<TModel> builder = new AddBuilder<TModel>(model, true);
			var translationResult = builder.Build();
			var executeResult = _executeCore.Execute(ExecuteType.INSERT, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
			model.Id = (Int32)executeResult.Value;
			return model;
		}
	}
}
