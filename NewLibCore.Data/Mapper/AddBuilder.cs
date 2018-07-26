using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper
{
	internal class AddBuilder<TModel> : SqlBuilder<TModel> where TModel : PropertyMonitor, new()
	{
		private Boolean _isVerifyModel;

		public AddBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
		{
			_isVerifyModel = isVerifyModel;
		}

		protected internal override BuildEntry<TModel> Build()
		{
			var buildEntry = new BuildEntry<TModel>(ModelInstance);
			var columns = ModelType.GetProperties().Where(w => w.PropertyType.Name != "IList`1" && w.GetCustomAttributes<ValidateBase>().Any());

			if (!columns.Any())
			{
				throw new Exception($@"{ModelType.Name}:没有要插入的列");
			}

			if (_isVerifyModel)
			{
				ValidateModel(columns.ToList());
			}

			buildEntry.Append($@" INSERT {ModelType.Name} ({String.Join(",", columns.Select(c => c.Name))} ) VALUES ({String.Join(",", columns.Select(key => $@"@{key.Name}"))})");
			buildEntry.AppendParameter(columns.ToList());
			return buildEntry;
		}
	}
}
