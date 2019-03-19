using System;

namespace NewLibCore.Data.SQL.MapperExtension.AssociationMapperExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]

    public class PrimaryKeyAttribute : Attribute
    {
    }
}
