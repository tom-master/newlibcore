using System;
using System.Runtime.Caching;

namespace NewLibCore.Data.SQL.Mapper.Cache
{
    /// <summary>
    /// 提供对sql查询结果的缓存操作
    /// </summary>
    public abstract class ResultCache
    {
        /// <summary>
        /// 初始化ResultCache类的新实例
        /// </summary>
        public ResultCache() { }

        /// <summary>
        /// 添加一个执行结果缓存
        /// </summary>
        /// <param name="key">缓存键</param>
        /// <param name="obj">缓存值</param>
        /// <param name="timeOut">超时时间</param>
        protected internal abstract void Add(String key, Object obj, DateTime? timeOut = null);

        /// <summary>
        /// 获取一个执行结果缓存
        /// </summary>
        /// <param name="key">缓存键</param>
        /// <returns></returns>
        protected internal abstract Object Get(String key);

        /// <summary>
        /// 使一个执行结果缓存失效
        /// </summary>
        /// <param name="key"></param>
        protected internal abstract void Remove(String key);
    }
    /// <summary>
    /// 提供对sql查询结果的缓存操作
    /// </summary>
    internal class DefaultResultCache : ResultCache
    {
        protected internal ObjectCache _baseCache;

        /// <summary>
        /// 初始化一个DefaultResultCache类的实例
        /// </summary>
        public DefaultResultCache()
        {
            _baseCache = MemoryCache.Default;
        }

        protected internal override void Add(String key, Object obj, DateTime? expire = null)
        {
            var cacheItem = new CacheItem(key, obj);
            var itemPolicy = new CacheItemPolicy
            {
                AbsoluteExpiration = expire ?? DateTime.Now.AddHours(1)
            };
            _baseCache.Add(cacheItem, itemPolicy);
        }

        protected internal override void Remove(String key)
        {
            _baseCache.Remove(key);
        }

        protected internal override Object Get(String key)
        {
            return _baseCache.Get(key);
        }
    }
}
