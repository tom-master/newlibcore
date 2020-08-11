using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Store;

namespace NewLibCore.Data.SQL.ProcessorFactory
{
    internal class RawSqlProcessor : Processor
    {
        public RawSqlProcessor(ExpressionProcessor expressionProcessor) : base(null, expressionProcessor)
        {
        }

        protected override ExecuteResult Execute(ExpressionStore store)
        {
            var result = _expressionProcessor.Parse(new ParseModel
            {
                Sql = store.RawSql.Sql,
                Parameters = store.RawSql.Parameters
            });
            return result.Execute();
        }
    }
}