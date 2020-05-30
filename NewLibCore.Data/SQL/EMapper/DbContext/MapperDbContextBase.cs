using System;
using System.Collections.Generic;
using System.Data.Common;

namespace NewLibCore.Data.SQL
{
    /// <summary>
    /// 执行解析后的SQL
    /// </summary>
    internal abstract class MapperDbContextBase : IDisposable
    {
        /// <summary>
        /// 是否使用事物
        /// </summary>
        /// <value></value>
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
        protected internal abstract ExecuteResult RawExecute(String sql, params MapperParameter[] parameters);

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
    }
}
