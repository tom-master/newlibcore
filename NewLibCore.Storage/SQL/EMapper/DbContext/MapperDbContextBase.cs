using System;
using System.Data.Common;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 执行解析后的SQL
    /// </summary>
    public abstract class MapperDbContextBase : IDisposable
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

        protected internal abstract ExecutorResult Insert(String sql, params MapperParameter[] parameters);
        protected internal abstract ExecutorResult Select(String sql, params MapperParameter[] parameters);
        protected internal abstract ExecutorResult Update(String sql, params MapperParameter[] parameters);

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
    }
}
