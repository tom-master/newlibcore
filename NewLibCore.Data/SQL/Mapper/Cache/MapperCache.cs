using System;
using System.Runtime.Caching;

namespace NewLibCore.Data.SQL.Mapper.Cache
{
    internal abstract class MapperCache
    {
        protected internal abstract void Add(String key, Object obj, TimeSpan? timeOut = null);

        protected internal abstract Object Get(String key);

        protected internal abstract void CacheInvalid(String key);
    }

    internal class StatementCache : MapperCache
    {
        protected internal ObjectCache _baseCache;

        protected internal StatementCache()
        {
            _baseCache = MemoryCache.Default;
        }

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
