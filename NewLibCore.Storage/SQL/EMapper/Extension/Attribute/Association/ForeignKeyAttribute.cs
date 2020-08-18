using System;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Validate
{
    /// <summary>
    /// 外键特性
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public class ForeignKeyAttribute : Attribute
    {
        public Type ForeignType { get; set; }

        public ForeignKeyAttribute(Type foreignType)
        {
            Parameter.IfNullOrZero(foreignType);
            ForeignType = foreignType;
        }
    }
}
