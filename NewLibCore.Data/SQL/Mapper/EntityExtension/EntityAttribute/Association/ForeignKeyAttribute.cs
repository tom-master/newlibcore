using System;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension.EntityAttribute.Association
{
    /// <summary>
    /// 外键特性
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public class ForeignKeyAttribute : Attribute
    {
    }
}
