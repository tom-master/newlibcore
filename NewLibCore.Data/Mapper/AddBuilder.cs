using System;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.Mapper.BuildExtension;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper
{
	internal class AddBuilder<TModel> : SqlBuilder<TModel> where TModel : PropertyMonitor, new()
	{
		private Boolean _isVerifyModel;

		private static readonly String _maxIdentity = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ";

		internal AddBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
		{
			_isVerifyModel = isVerifyModel;
		}

		protected internal override BuildEntry<TModel> Build()
		{
			var buildEntry = new BuildEntry<TModel>(ModelInstance);
			var columns = ModelType.GetProperties().Where(w => w.GetCustomAttributes<ValidateBase>().Any());

			if (!columns.Any())
			{
				throw new Exception($@"{ModelType.Name}:没有要插入的列");
			}

			if (_isVerifyModel)
			{
				ValidateModel(columns.ToList());
			}

			buildEntry.Append($@" INSERT {ModelType.Name} ({String.Join(",", columns.Select(c => c.Name))} ) VALUES ({String.Join(",", columns.Select(key => $@"@{key.Name}"))}) {_maxIdentity} ");
			buildEntry.AppendParameter(columns.ToList().Select(c => new ParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))));
			
			return buildEntry;
		}
	}
}
