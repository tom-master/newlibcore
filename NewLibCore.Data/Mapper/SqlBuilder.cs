using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using NewLibCore.Data.Mapper.MapperExtension;

namespace NewLibCore.Data.Mapper
{
	internal abstract class SqlBuilder<TModel> where TModel : class,new()
	{
		private StringBuilder _sqlBuilder = new StringBuilder();
		private Type _modelType;

		private TModel _modelInstance;

		protected SqlBuilder(TModel model)
		{
			if (model == null)
			{
				throw new Exception();
			}

			_modelType = model.GetType();
			_modelInstance = model;
		}

		protected internal abstract BuildEntry<TModel> Build();

		protected Type ModelType
		{
			get { return _modelType; }
		}

		protected TModel ModelInstance
		{
			get { return _modelInstance; }
		}

	}
}
