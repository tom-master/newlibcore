using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.EMapper.Parser
{
    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal abstract class ConditionProcessor
    {
        protected readonly ProcessExecutor _processorResult;

        internal ConditionProcessor(ProcessExecutor processorResult)
        {
            _processorResult = processorResult;
        }

        internal ProcessExecutor Process(JoinComponent joinComponent, WhereComponent whereComponent, FromComponent fromComponent)
        {
            return _processorResult;
        }

        protected abstract ProcessExecutor InnerProcess(JoinComponent joinComponent, WhereComponent whereComponent, FromComponent fromComponent);
    }
}
