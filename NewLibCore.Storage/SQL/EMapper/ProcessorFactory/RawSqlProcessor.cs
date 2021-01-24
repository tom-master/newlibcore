using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper.Parser;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    internal class RawSqlProcessor : Processor
    {
        public RawSqlProcessor(ConditionProcessor conditionProcessor) : base(conditionProcessor)
        {
        }

        protected override SqlExecuteResultConvert Execute(SqlComponent sqlComponent)
        {
            var result = ConditionProcessor.Process(new ParseModel
            {
                Sql = sqlComponent.RawSql.Sql,
                Parameters = sqlComponent.RawSql.Parameters
            });
            return result.Execute();
        }
    }
}