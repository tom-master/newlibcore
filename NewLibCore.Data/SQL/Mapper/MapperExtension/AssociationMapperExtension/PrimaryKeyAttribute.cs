using System;

namespace NewLibCore.Data.SQL.Mapper.Extension.AssociationMapperExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]

    public class PrimaryKeyAttribute : Attribute
    {
    }
}
