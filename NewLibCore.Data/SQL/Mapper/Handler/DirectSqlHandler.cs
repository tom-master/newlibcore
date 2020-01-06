using System;
using System.Collections.Generic;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    internal class DirectSqlHandler
    {
        private readonly TemplateBase _templateBase;

        private readonly ParserExecutor _parserExecutor;

        public DirectSqlHandler(TemplateBase templateBase, ParserExecutor parserExecutor)
        {
            Parameter.Validate(templateBase);
            Parameter.Validate(parserExecutor);
        }

        internal ExecuteResult Execute(String sql, params MapperParameter[] parameters)
        {
            var result = _parserExecutor.Parse(new ParseModel
            {
                Sql = sql,
                Parameters = parameters
            });
            return result.Execute();
        }
    }
}