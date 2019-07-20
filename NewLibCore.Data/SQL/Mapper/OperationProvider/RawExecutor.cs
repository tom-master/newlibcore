using System;
using System.Collections.Generic;
using System.Data;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.InternalExtension;

namespace NewLibCore.Data.SQL.Mapper.OperationProvider
{
    internal class RawExecutor : IRawExecutor
    {
        private ExecuteCore _executeCore;

        public RawExecutor(ExecuteCore executeCore)
        {
            _executeCore = executeCore;
        }

        /// <summary>
        /// 获取一个TModel的对象列表
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public List<TModel> ToList<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            var dataTable = (DataTable)ExecuteRawSql(ExecuteType.SELECT, sql, parameters).Value;
            return dataTable.ToList<TModel>();
        }

        /// <summary>
        /// 获取一个TModel对象
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel ToSingle<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            var modelType = typeof(TModel);
            RawExecuteResult executeResult;
            if (modelType.IsNumeric())
            {
                executeResult = ExecuteRawSql(ExecuteType.SELECT_SINGLE, sql, parameters);
                return (TModel)Convert.ChangeType(executeResult.Value, modelType);
            }

            executeResult = ExecuteRawSql(ExecuteType.SELECT, sql, parameters);
            var dataTable = (DataTable)executeResult.Value;
            return dataTable.ToSingle<TModel>();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="executeType"></param>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <returns></returns>
        private RawExecuteResult ExecuteRawSql(ExecuteType executeType, String sql, IEnumerable<EntityParameter> parameters = null)
        {
            return _executeCore.RawExecute(executeType, sql, parameters);
        }
    }
}
