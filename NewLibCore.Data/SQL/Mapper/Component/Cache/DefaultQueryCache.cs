using System;
using System.Runtime.Caching;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Component.Cache
{
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
                AbsoluteExpiration = expire ?? DateTime.Now.AddHours(1),
            };
            _baseCache.Set(cacheItem, itemPolicy);
        }

        public override void Remove(String key)
        {
            Parameter.Validate(key);
            _baseCache.Remove(key);
        }

        public override TResult Get<TResult>(String key)
        {
            Parameter.Validate(key);
            var result = _baseCache.Get(key);
            if (result == null)
            {
                return default(TResult);
            }
            return (TResult)result;
        }
    }
}
