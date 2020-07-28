using System;

namespace NewLibCore.Data.SQL.Validate
{
    /// <summary>
    /// 忽略被监视的属性
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
    class IgnoreMonitorAttribute : Attribute
    {
    }
}
