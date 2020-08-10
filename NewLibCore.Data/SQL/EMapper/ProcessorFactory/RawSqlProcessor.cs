using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Template;

namespace NewLibCore.Data.SQL.ProcessorFactory
{
    internal class RawSqlProcessor : Processor
    {
        public RawSqlProcessor(TemplateBase templateBase, ParserExecutor parserExecutor) : base(templateBase, parserExecutor)
        {
        }

        protected override ExecuteResult Execute(ExpressionStore store)
        {
            var result = _parserExecutor.Parse(new ParseModel
            {
                Sql = store.RawSql.Sql,
                Parameters = store.RawSql.Parameters
            });
            return result.Execute();
        }
    }
}