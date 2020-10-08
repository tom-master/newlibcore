using System;
namespace NewLibCore.Storage.SQL.Component.Cache
{
    public class RedisQueryCache : QueryCacheBase
    {
        public RedisQueryCache()
        {

        }

        public override void Add(string key, object obj, TimeSpan? timeOut = default)
        {

        }

        public override TResult Get<TResult>(string key)
        {
            return default;
        }

        public override void Remove(string key)
        {
        }
    }
}