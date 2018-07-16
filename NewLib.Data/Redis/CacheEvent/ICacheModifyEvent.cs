using System;

namespace NewLib.Data.Redis.CacheEvent
{
    internal interface ICacheModifyEvent
    {
        event EventHandler OnCacheModify;

        void CacheModify();
    }
}
