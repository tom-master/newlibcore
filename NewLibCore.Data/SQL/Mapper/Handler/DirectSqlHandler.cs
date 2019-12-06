using System;
using System.Collections.Generic;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    internal class DirectSqlHandler : HandlerBase
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

        protected override ExecuteResult Execute()
        {
            ParserResult.Append(_sql, _parameters);
            return ParserResult.Execute();
        }
    }
}