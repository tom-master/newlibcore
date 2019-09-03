using System;
using System.Runtime.Caching;

namespace NewLibCore.Data.SQL.Mapper.Cache
{
    /// <summary>
    /// 映射缓存
    /// </summary>
    internal abstract class ResultCache
    {
        public ResultCache() { }

        /// <summary>
        /// 添加一个执行结果缓存
        /// </summary>
        /// <param name="key">缓存键</param>
        /// <param name="obj">缓存值</param>
        /// <param name="timeOut">超时时间</param>
        protected internal abstract void Add(String key, Object obj, TimeSpan? timeOut = null);

        /// <summary>
        /// 获取一个执行结果缓存
        /// </summary>
        /// <param name="key">缓存键</param>
        /// <returns></returns>
        protected internal abstract Object Get(String key);

        /// <summary>
        /// 使一个执行结果缓存失效
        /// </summary>
        /// <param name="key">缓存键</param>
        protected internal abstract void CacheInvalid(String key);
    }
    /// <summary>
    /// 缓存sql执行的结果
    /// </summary>
    internal class ExecutionResultCache : ResultCache
    {
        protected internal ObjectCache _baseCache;

        /// <summary>
        /// 初始化一个ExecutionResultCache类的实例
        /// </summary>
        public ExecutionResultCache()
        {
            _baseCache = MemoryCache.Default;
        }

        protected internal override void Add(String key, Object obj, DateTime? timeOut = null)
        {
            var alive = new Random(DateTime.Now.Millisecond).Next(1, 3);
            var cacheItem = new CacheItem(key, obj);
            var itemPolicy = new CacheItemPolicy
            {
                AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(alive)
            };
            _baseCache.Add(cacheItem, itemPolicy);
        }

        protected internal override void CacheInvalid(String key)
        {
            _baseCache.Remove(key);
        }

        protected internal override Object Get(String key)
        {
            return _baseCache.Get(key);
        }
    }
}
