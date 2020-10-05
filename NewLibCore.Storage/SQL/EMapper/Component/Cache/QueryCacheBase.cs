using System;

namespace NewLibCore.Storage.SQL.Component.Cache
{
    /// <summary>
    /// 提供对sql查询结果的缓存操作
    /// </summary>
    public abstract class QueryCacheBase
    {
        /// <summary>
        /// 添加一个执行结果缓存
        /// </summary>
        /// <param name="key">缓存键</param>
        /// <param name="obj">缓存值</param>
        /// <param name="timeOut">超时时间</param>
        public abstract void Add(String key, Object obj, DateTime? timeOut = null);

        /// <summary>
        /// 获取一个执行结果缓存
        /// </summary>
        /// <param name="key"></param>
        /// <typeparam name="TResult"></typeparam>
        /// <returns></returns>
        public abstract TResult Get<TResult>(String key);

        /// <summary>
        /// 使一个执行结果缓存失效
        /// </summary>
        /// <param name="key"></param>
        public abstract void Remove(String key);
    }
}
