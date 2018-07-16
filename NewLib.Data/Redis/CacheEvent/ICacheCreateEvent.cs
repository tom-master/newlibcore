using System;

namespace NewLib.Data.Redis.CacheEvent
{
    internal interface ICacheCreateEvent
    {
        event EventHandler OnCacheCreate;

        void CacheCreate();
    }
}
