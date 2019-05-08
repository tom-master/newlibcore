using System;
using System.Runtime.Caching;

namespace NewLibCore.Data.SQL.Mapper.Cache
{
    internal abstract class MapperCache
    {
        protected internal abstract void Add(String key, Object obj, TimeSpan? timeOut = null);

        protected internal abstract Object Get(String key);
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
            var cacheItem = new CacheItem(key, obj);
            var itemPolicy = new CacheItemPolicy
            {
                AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(new Random(DateTime.Now.Millisecond).Next(5, 60))
            };
            _baseCache.Add(cacheItem, itemPolicy);
        }

        protected internal override Object Get(String key)
        {
            return _baseCache.Get(key);
        }
    }
}
