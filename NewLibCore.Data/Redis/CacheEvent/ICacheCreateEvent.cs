using System;

namespace NewLibCore.Data.Redis.CacheEvent
{
    internal interface ICacheCreateEvent
    {
        event EventHandler OnCacheCreate;

        void CacheCreate();
    }
}
