using System;

namespace NewLibCore.Data.Redis.CacheEvent
{
    internal interface ICacheModifyEvent
    {
        event EventHandler OnCacheModify;

        void CacheModify();
    }
}
