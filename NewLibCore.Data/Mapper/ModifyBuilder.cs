using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper
{
	internal class ModifyBuilder<TModel> : SqlBuilder<TModel> where TModel : PropertyMonitor, new()
	{
		private Expression<Func<TModel, Boolean>> _where;
		private Boolean _isValidate;

		public ModifyBuilder(TModel model, Expression<Func<TModel, Boolean>> where = null, Boolean isValidate = false) : base(model)
		{
			_where = where;
			_isValidate = isValidate;
		}

		protected internal override BuildEntry<TModel> Build()
		{
			if (!ModelInstance.Args.Any())
			{
				throw new ArgumentNullException("没有找到需要更新的字段");
			}

			var columns = ModelInstance.Args;
			if (_isValidate)
			{
				ValidateModel(ModelInstance.Args.Select(s => s.PropertyInfo).ToList());
			}

			var buildEntry = new BuildEntry<TModel>(ModelInstance);
			buildEntry.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", columns.Select(s => $@"{s.PropertyName}=@{s.PropertyName}"))}");
			if (_where != null)
			{
				buildEntry.BuildWhere(_where);
			}

			foreach (var item in columns.Select(s => new ParameterMapper($@"@{s.PropertyName}", s.GetPropertyValue(ModelInstance))))
			{
				buildEntry.Parameters.Add(item);
			}
			return buildEntry;
		}
	}
}
