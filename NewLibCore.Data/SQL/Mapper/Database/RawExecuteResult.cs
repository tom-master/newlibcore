using System;
using System.Data;

namespace NewLibCore.Data.SQL.Mapper.Database
{
    /// <summary>
    /// 存储未包装过的sql执行结果
    /// </summary>
    internal class RawExecuteResult
    {
        private Object _result;

        internal Object Result
        {
            get
            {
                return _result;
            }
            set
            {
                _result = value;
            }
        }

        internal RawExecuteResult()
        {
        }
    }
}
