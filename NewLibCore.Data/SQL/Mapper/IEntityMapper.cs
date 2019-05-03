using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.InternalExtension;
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

		ISelectEntityMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression = null) where T : EntityBase, new();

		ISelectEntityMapper<TModel> Page(Int32 pageIndex, Int32 pageSize);

		ISelectEntityMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

		ISelectEntityMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

		ISelectEntityMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

		ISelectEntityMapper<TModel> OrderBy<TOrder, TKey>(Expression<Func<TOrder, TKey>> order, OrderByType orderBy = OrderByType.DESC) where TOrder : EntityBase, new();
	}

	public interface ISqlExecutor<TModel> where TModel : EntityBase, new()
	{
		TModel Execute(String sql, IEnumerable<EntityParameter> parameters = null);
	}

	public class SqlExecutor<TModel> : ISqlExecutor<TModel> where TModel : EntityBase, new()
	{
		private ExecuteCore _executeCore;

		public SqlExecutor(ExecuteCore executeCore)
		{
			_executeCore = executeCore;
		}

		public TModel Execute(String sql, IEnumerable<EntityParameter> parameters = null)
		{
			ExecuteCoreResult executeResult;
			if (typeof(TModel).IsNumeric())
			{
				executeResult = _executeCore.Execute(ExecuteType.SELECT_SINGLE, sql, parameters, CommandType.Text);
				return (TModel)executeResult.Value;
			}
			executeResult = _executeCore.Execute(ExecuteType.SELECT, sql, parameters, CommandType.Text);
			var dataTable = (DataTable)executeResult.Value;
			return (TModel)dataTable.AsList<TModel>();
		}
	}

	public class SelectEntityMapper<TModel> : ISelectEntityMapper<TModel> where TModel : EntityBase, new()
	{
		private ExecuteCore _execute;
		private StatementStore _statementStore;

		public SelectEntityMapper(ExecuteCore executeCore)
		{
			_execute = executeCore;
			_statementStore = new StatementStore();
		}

		public TModel ToOne()
		{
			return ToList().FirstOrDefault();
		}

		public IList<TModel> ToList()
		{
			IBuilder<TModel> builder = new SelectBuilder<TModel>(_statementStore);
			var executeResult = _execute.Execute(ExecuteType.SELECT, builder.Build());
			var dataTable = executeResult.Value as DataTable;
			return dataTable.AsList<TModel>();
		}

		public Int32 Count(Expression<Func<TModel, Boolean>> where = null)
		{
			if (where != null)
			{
				_statementStore.Add(where);
			}
			return 0;
		}

		public ISelectEntityMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
		{
			if (fields != null)
			{
				_statementStore.Add(fields);
			}
			return this;
		}

		public ISelectEntityMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression = null) where T : EntityBase, new()
		{
			if (expression != null)
			{
				_statementStore.Add(expression);
			}
			return this;
		}

		public ISelectEntityMapper<TModel> Page(Int32 pageIndex, Int32 pageSize)
		{
			_statementStore.AddPage(pageIndex, pageSize);
			return this;
		}

		public ISelectEntityMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
		{
			Parameter.Validate(expression);
			_statementStore.Add(expression, JoinType.LEFT);
			return this;
		}

		public ISelectEntityMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
		{
			Parameter.Validate(expression);
			_statementStore.Add(expression, JoinType.RIGHT);
			return this;
		}

		public ISelectEntityMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
		{
			Parameter.Validate(expression);
			_statementStore.Add(expression, JoinType.INNER);
			return this;
		}

		public ISelectEntityMapper<TModel> OrderBy<TOrder, TKey>(Expression<Func<TOrder, TKey>> order, OrderByType orderBy = OrderByType.DESC) where TOrder : EntityBase, new()
		{
			Parameter.Validate(order);
			_statementStore.AddOrderBy(order, orderBy);
			return this;
		}
	}

	public class UpdateEntityMapper<TModel> : IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
	{
		private ExecuteCore _execute;
		private StatementStore _statementStore;

		public UpdateEntityMapper(ExecuteCore executeCore)
		{
			_execute = executeCore;
			_statementStore = new StatementStore();
		}

		public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
		{
			using (_execute)
			{
				_statementStore.Add(expression);
				IBuilder<TModel> builder = new ModifyBuilder<TModel>(model, _statementStore, true);
				var executeResult = _execute.Execute(ExecuteType.UPDATE, builder.Build());
				return (Int32)executeResult.Value > 0;
			}
		}
	}

	public class AddEntityMapper<TModel> : IAddEntityMapper<TModel> where TModel : EntityBase, new()
	{
		private readonly ExecuteCore _executeCore;

		public AddEntityMapper(ExecuteCore executeCore)
		{
			_executeCore = executeCore;
		}

		public TModel Add(TModel model)
		{
			IBuilder<TModel> builder = new AddBuilder<TModel>(model, true);
			var executeResult = _executeCore.Execute(ExecuteType.INSERT, builder.Build());
			model.Id = (Int32)executeResult.Value;
			return model;
		}
	}
}
