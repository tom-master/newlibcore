using System;

namespace NewLibCore.Storage.SQL.Validate
{
    /// <summary>
    /// 主键特性
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
    public class PrimaryKeyAttribute : PropertyValidateAttribute
    {
    }
}
