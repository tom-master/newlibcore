using System;
namespace NewLibCore.Storage.SQL.Component.Cache
{
    public class RedisQueryCache : QueryCacheBase
    {
        public RedisQueryCache()
        {
        }

        public override void Add(string key, object obj, DateTime? timeOut = null)
        {
        }

        public override TResult Get<TResult>(string key)
        {
        }

        public override void Remove(string key)
        {
        }
    }
}