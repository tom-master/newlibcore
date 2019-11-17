using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper.Database
{
    internal abstract class MapperDbContextBase : IDisposable
    {

        protected internal Boolean UseTransaction { get; set; }

        /// <summary>
        /// 提交一个事物
        /// </summary>
        protected internal abstract void Commit();

        /// <summary>
        /// 回滚一个事物
        /// </summary>
        protected internal abstract void Rollback();

        /// <summary>
        /// 打开连接
        /// </summary>
        protected internal abstract void OpenConnection();

        /// <summary>
        /// 打开事务
        /// </summary>
        /// <returns></returns>
        protected internal abstract DbTransaction OpenTransaction();

        /// <summary>
        /// 释放资源
        /// </summary>
        /// <param name="disposing"></param>

        protected internal abstract void Dispose(Boolean disposing);

        /// <summary>
        /// 获取语句的执行类型
        /// </summary>
        /// <param name="sql">语句</param>
        /// <returns></returns>
        protected internal abstract ExecuteType GetExecuteType(String sql);

        /// <summary>
        /// 执行原生sql语句
        /// </summary>
        /// <param name="executeType">执行的类型</param>
        /// <param name="sql">语句</param>
        /// <param name="parameters">参数</param>
        /// <param name="commandType"></param>
        /// <returns></returns>
        protected internal abstract RawResult RawExecute(String sql, IEnumerable<EntityParameter> parameters = null, CommandType commandType = CommandType.Text);

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
    }
}
