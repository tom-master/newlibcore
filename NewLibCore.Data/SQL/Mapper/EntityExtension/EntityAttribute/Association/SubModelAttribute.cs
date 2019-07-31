using System;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension.EntityAttribute.Association
{
    /// <summary>
    /// 关联子表特性
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public class SubModelAttribute : Attribute
    {
    }
}
