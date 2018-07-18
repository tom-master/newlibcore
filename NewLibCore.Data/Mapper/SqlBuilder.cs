using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using NewLibCore.Data.Mapper.MapperExtension;

namespace NewLibCore.Data.Mapper
{
	internal abstract class SqlBuilder<TModel> where TModel : class, new()
	{
		private StringBuilder _sqlBuilder = new StringBuilder();

		protected Type ModelType { get; }

		protected TModel ModelInstance { get; }

		protected SqlBuilder(TModel model)
		{
			if (model == null)
			{
				ModelInstance = Activator.CreateInstance<TModel>();
				ModelType = typeof(TModel);
			}
			else
			{
				ModelInstance = model;
				ModelType = model.GetType();
			}

		}

		protected internal abstract BuildEntry<TModel> Build();
	}
}
