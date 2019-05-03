using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
	public sealed class EntityMapper : IDisposable
	{
		private readonly ExecuteCore _executeCore;

		public EntityMapper()
		{
			_executeCore = new ExecuteCore();
		}

		public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
		{
			Parameter.Validate(model);
			return new AddEntityMapper<TModel>(_executeCore).Add(model);
		}

		public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
		{
			Parameter.Validate(model);
			Parameter.Validate(expression);
			return new UpdateEntityMapper<TModel>(_executeCore).Update(model, expression);
		}

		public ISelectEntityMapper<TModel> Select<TModel>(Expression<Func<TModel, dynamic>> fields = null) where TModel : EntityBase, new()
		{
			return new SelectEntityMapper<TModel>(_executeCore).Select(fields);
		}

		public void Dispose()
		{
			_executeCore.Dispose();
		}
	}
}
