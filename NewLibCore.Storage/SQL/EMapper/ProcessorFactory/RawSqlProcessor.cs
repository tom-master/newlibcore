using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Store;

namespace NewLibCore.Data.SQL.ProcessorFactory
{
    internal class RawSqlProcessor : Processor
    {
        public RawSqlProcessor(ExpressionProcessor expressionProcessor) : base(expressionProcessor)
        {
        }

        protected override SqlExecuteResultConvert Execute(ExpressionStore store)
        {
            var result = _expressionProcessor.Processor(new ParseModel
            {
                Sql = store.RawSql.Sql,
                Parameters = store.RawSql.Parameters
            });
            return result.Execute();
        }
    }
}