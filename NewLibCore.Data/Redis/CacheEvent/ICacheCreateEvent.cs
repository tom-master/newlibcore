using System;

namespace NewLibCore.Data.Redis.CacheEvent
{
    public abstract class CacheEvent
    {
        protected abstract event EventHandler OnCreate;
          
        protected abstract event EventHandler OnModify;

    }
}
