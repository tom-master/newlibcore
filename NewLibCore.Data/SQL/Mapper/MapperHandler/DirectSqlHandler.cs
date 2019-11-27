using System;
using System.Collections.Generic;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    internal class DirectSqlHandler : Handler
    {
        private readonly String _sql;

        private readonly IEnumerable<MapperParameter> _parameters;

        public DirectSqlHandler(String sql, IServiceProvider serviceProvider) : this(sql, null, serviceProvider)
        {

        }

        public DirectSqlHandler(String sql, IEnumerable<MapperParameter> parameters, IServiceProvider serviceProvider) : base(serviceProvider)
        {
            Parameter.Validate(sql);

            _sql = sql;
            _parameters = parameters;
        }

        internal override RawResult Execute()
        {
            var sqlResult = ParserResult.CreateResult();
            sqlResult.Append(_sql, _parameters);
            return sqlResult.Execute(ServiceProvider);
        }
    }
}