using System;

namespace NewLibCore.Data.SQL.Mapper.Execute
{
    /// <summary>
    /// 存储未包装过的sql执行结果
    /// </summary>
    internal class RawExecuteResult
    {
        internal Object Value { get; set; }

        internal RawExecuteResult()
        {
        }
    }
}
