using System;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
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
            Parameter.Validate(foreignType);
            ForeignType = foreignType;
        }
    }
}
