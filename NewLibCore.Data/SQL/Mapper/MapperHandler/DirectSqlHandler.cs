using System;
using System.Collections.Generic;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper
{
    internal class DirectSqlHandler : Handler
    {
        private readonly String _sql;

        private readonly IEnumerable<EntityParameter> _parameters;

        public DirectSqlHandler(String sql, IServiceProvider serviceProvider) : base(serviceProvider)
        {
            _sql = sql;
        }

        public DirectSqlHandler(String sql, IEnumerable<EntityParameter> parameters, IServiceProvider serviceProvider) : base(serviceProvider)
        {
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