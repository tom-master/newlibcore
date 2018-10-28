using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;
using NewLibCore.Data.Mapper.InternalDataStore;

namespace NewLibCore.Data.Mapper.BuildExtension
{
	internal class BuildEntry<TModel> where TModel : class, new()
	{
		private StringBuilder _builder;

		private TModel _model;

		internal BuildEntry(TModel model)
		{
			_model = model;
			_builder = new StringBuilder();
		}

		public List<ParameterMapper> ParameterMappers { get; private set; } = new List<ParameterMapper>();

		internal void Append(String value)
		{
			_builder.Append(value);
		}

		internal void AppendParameter(IList<PropertyInfo> propertyInfos)
		{
			var parameters = propertyInfos.ToList().Select(c => new ParameterMapper($@"@{c.Name}", c.GetValue(_model)));
			ParameterMappers.AddRange(parameters);
		}

		public string FormatSql()
		{
			return _builder.ToString();
		}
	}
}
