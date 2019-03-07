using NewLibCore.Data.SQL.InternalDataStore;
using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal class BuildEntry<TModel> where TModel : class, new()
    {
        private readonly TModel _model;
        private StringBuilder _builder;

        private List<SqlParameterMapper> _parameterMappers = new List<SqlParameterMapper>();

        internal BuildEntry(TModel model)
        {
            _model = model;
            _builder = new StringBuilder();
        }

        internal void Append(String value)
        {
            _builder.Append(value);
        }

        internal void AppendParameter(IEnumerable<SqlParameterMapper> mappers)
        {
            _parameterMappers.AddRange(mappers);
        }

        internal List<SqlParameterMapper> GetParameters()
        {
            return _parameterMappers;
        }

        public String ToSql()
        {
            return _builder.ToString();
        }
    }
}
