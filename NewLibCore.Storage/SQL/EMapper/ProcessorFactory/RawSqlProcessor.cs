using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Store;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    internal class RawSqlProcessor : Processor
    {
        public RawSqlProcessor(ConditionProcessor conditionProcessor) : base(conditionProcessor)
        {
        }

        protected override SqlExecuteResultConvert Execute(ExpressionStore store)
        {
            var result = _conditionProcessor.Process(new ParseModel
            {
                Sql = store.RawSql.Sql,
                Parameters = store.RawSql.Parameters
            });
            return result.Execute();
        }
    }
}