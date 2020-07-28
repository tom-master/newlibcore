using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore.Data.SQL.EMapper.Extension
{
    /// <summary>
    /// 忽略被监视的属性
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
    class IgnoreMonitorAttribute : Attribute
    {
    }
}
