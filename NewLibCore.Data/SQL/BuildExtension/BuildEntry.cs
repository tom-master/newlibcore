using NewLibCore.Data.SQL.InternalDataStore;
using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
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

        internal void AppendParameter(IEnumerable<ParameterMapper> mappers)
        {
            ParameterMappers.AddRange(mappers);
        }

        public string FormatSql()
        {
            return _builder.ToString();
        }
    }
}
