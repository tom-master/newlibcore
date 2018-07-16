using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper
{
	internal class UpdateBuilder<TModel>: SqlBuilder<TModel> where TModel : PropertyMonitor, new()
	{
		private Expression<Func<TModel, Boolean>> _where;

		public UpdateBuilder(TModel model, Expression<Func<TModel, Boolean>> where = null) : base(model)
		{
			_where = where;
		}

		protected internal override BuildEntry Build()
		{
			if (!ModelInstance.Args.Any())
			{
				throw new ArgumentNullException("没有找到需要更新的字段");
			}

			var columns = ModelInstance.Args;
			var builder = new BuilderExtension<TModel>();
			builder.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", columns.Select(s => $@"{s.GetArgumentName()}=@{s.GetArgumentName()}"))}");
			if (_where != null)
			{
				builder.BuildWhere(_where);
			}

			var entry = builder.GetBuildEntry();
			foreach (var item in columns.Select(s => new ParameterMapper($@"@{s.GetArgumentName()}", s.PropertyValue)))
			{
				entry.Parameters.Add(item);
			}
			return entry;
		}
	}
}
