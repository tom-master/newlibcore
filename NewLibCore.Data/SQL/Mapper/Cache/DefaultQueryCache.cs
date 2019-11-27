using System;
using System.Runtime.Caching;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Cache
{
    /// <summary>
    /// 提供对sql查询结果的缓存操作
    /// </summary>
    public abstract class QueryCacheBase
    {
        /// <summary>
        /// 初始化ResultCache类的新实例
        /// </summary>
        public QueryCacheBase() { }

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
        /// <param name="key">缓存键</param>
        /// <returns></returns>
        public abstract Object Get(String key);

        /// <summary>
        /// 使一个执行结果缓存失效
        /// </summary>
        /// <param name="key"></param>
        public abstract void Remove(String key);
    }

    /// <summary>
    /// 提供对sql查询结果的缓存操作
    /// </summary>
    internal class DefaultQueryCache : QueryCacheBase
    {
        protected internal ObjectCache _baseCache;

        /// <summary>
        /// 初始化一个DefaultResultCache类的实例
        /// </summary>
        public DefaultQueryCache()
        {
            _baseCache = MemoryCache.Default;
        }

        public override void Add(String key, Object obj, DateTime? expire = null)
        {
            Parameter.Validate(key);
            Parameter.Validate(obj);

            var cacheItem = new CacheItem(key, obj);
            var itemPolicy = new CacheItemPolicy
            {
                AbsoluteExpiration = expire ?? DateTime.Now.AddHours(1)
            };
            _baseCache.Add(cacheItem, itemPolicy);
        }

        public override void Remove(String key)
        {
            Parameter.Validate(key);
            _baseCache.Remove(key);
        }

        public override Object Get(String key)
        {
            Parameter.Validate(key);
            return _baseCache.Get(key);
        }
    }
}
