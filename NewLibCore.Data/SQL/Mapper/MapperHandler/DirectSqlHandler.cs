using System;
using System.Collections.Generic;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper
{
    internal class DirectSqlHandler : Handler
    {
        private readonly String _sql;

        private readonly IEnumerable<EntityParameter> _parameters;

        private readonly IServiceProvider _serviceProvider;

        public DirectSqlHandler(String sql, IServiceProvider serviceProvider)
        {
            _sql = sql;
            _serviceProvider = serviceProvider;
        }

        public DirectSqlHandler(String sql, IEnumerable<EntityParameter> parameters, IServiceProvider serviceProvider)
        {
            _sql = sql;
            _parameters = parameters;
            _serviceProvider = serviceProvider;
        }

        internal override RawResult Execute()
        {
            var sqlResult = TranslateResult.CreateResult();
            sqlResult.Append(_sql, _parameters);
            return sqlResult.Execute(_serviceProvider);
        }
    }
}