using System;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Data.SQL.Mapper.Template;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    internal class DirectSqlHandler : HandlerBase
    {
        public DirectSqlHandler(TemplateBase templateBase, ParserExecutor parserExecutor) : base(templateBase, parserExecutor)
        {
        }

        protected override ExecuteResult Execute(ExpressionStore store)
        {
            var result = _parserExecutor.Parse(new ParseModel
            {
                Sql = store.Direct.Sql,
                Parameters = store.Direct.Parameters
            });
            return result.Execute();
        }
    }
}