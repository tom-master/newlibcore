using System;
using System.Collections.Generic;
using System.Data;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.InternalExtension;

namespace NewLibCore.Data.SQL.Mapper
{
	public sealed class EntityMapper : IDisposable
	{
		private readonly ExecuteCore _executeCore;

		public EntityMapper()
		{
			_executeCore = new ExecuteCore();
		}

		public ISelectEntityMapper<TModel> CreateSelect<TModel>() where TModel : EntityBase, new()
		{
			return new SelectEntityMapper<TModel>();
		}

		public IAddEntityMapper<TModel> CreateAdd<TModel>() where TModel : EntityBase, new()
		{
			return new AddEntityMapper<TModel>();
		}

		public TModel ComplexSqlExecute<TModel>(String sql, IEnumerable<EntityParameter> sqlParameters = null) where TModel : new()
		{
			ExecuteCoreResult executeResult;
			if (typeof(TModel).IsNumeric())
			{
				executeResult = _executeCore.Execute(ExecuteType.SELECT_SINGLE, sql, sqlParameters, CommandType.Text);
				return (TModel)executeResult.Value;
			}
			executeResult = _executeCore.Execute(ExecuteType.SELECT, sql, sqlParameters, CommandType.Text);
			var dataTable = (DataTable)executeResult.Value;
			return (TModel)dataTable.AsList<TModel>();
		}

		public void Dispose()
		{
			_executeCore.Dispose();
		}
	}
}
