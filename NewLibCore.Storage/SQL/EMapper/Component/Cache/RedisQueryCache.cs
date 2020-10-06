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
            throw new NotImplementedException();
        }

        public override TResult Get<TResult>(string key)
        {
            throw new NotImplementedException();
        }

        public override void Remove(string key)
        {
            throw new NotImplementedException();
        }
    }
}