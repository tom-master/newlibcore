using System;
using System.Runtime.Caching;

namespace NewLibCore.Data.SQL.Mapper.Cache
{
    /// <summary>
    /// 映射缓存
    /// </summary>
    internal abstract class ResultCache
    {
        /// <summary>
        /// 添加一个执行结果缓存
        /// </summary>
        /// <param name="key"></param>
        /// <param name="obj"></param>
        /// <param name="timeOut"></param>
        protected internal abstract void Add(String key, Object obj, TimeSpan? timeOut = null);

        /// <summary>
        /// 获取一个执行结果缓存
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        protected internal abstract Object Get(String key);

        /// <summary>
        /// 使一个执行结果缓存失效
        /// </summary>
        /// <param name="key"></param>
        protected internal abstract void CacheInvalid(String key);
    }
    /// <summary>
    /// 缓存sql执行的结果
    /// </summary>
    internal class ExecutionResultCache : ResultCache
    {
        protected internal ObjectCache _baseCache;

        protected internal ExecutionResultCache()
        {
            _baseCache = MemoryCache.Default;
        }

        /// <summary>
        /// 添加一个执行结果缓存
        /// </summary>
        /// <param name="key"></param>
        /// <param name="obj"></param>
        /// <param name="timeOut"></param>
        protected internal override void Add(String key, Object obj, TimeSpan? timeOut = null)
        {
            var alive = new Random(DateTime.Now.Millisecond).Next(1, 3);
            var cacheItem = new CacheItem(key, obj);
            var itemPolicy = new CacheItemPolicy
            {
                AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(alive)
            };
            _baseCache.Add(cacheItem, itemPolicy);
        }

        /// <summary>
        /// 获取一个执行结果缓存
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        protected internal override void CacheInvalid(String key)
        {
            _baseCache.Remove(key);
        }

        /// <summary>
        /// 使一个执行结果缓存失效
        /// </summary>
        /// <param name="key"></param>
        protected internal override Object Get(String key)
        {
            return _baseCache.Get(key);
        }
    }
}
