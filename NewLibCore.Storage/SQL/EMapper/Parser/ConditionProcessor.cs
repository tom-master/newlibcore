using NewLibCore.Storage.SQL.Component.Sql;

namespace NewLibCore.Storage.SQL.EMapper.Parser
{
    internal abstract class ConditionProcessor
    {
        protected readonly ProcessExecutor _processorResult;

        public ConditionProcessor(ProcessExecutor processorResult)
        {
            _processorResult = processorResult;
        }

        internal ProcessExecutor Process(JoinComponent joinComponent, WhereComponent whereComponent, FromComponent fromComponent)
        {
            return InnerProcess(joinComponent, whereComponent, fromComponent);
        }

        protected abstract ProcessExecutor InnerProcess(JoinComponent joinComponent, WhereComponent whereComponent, FromComponent fromComponent);
    }
}
