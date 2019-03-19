using System;

namespace NewLibCore.Data.SQL.MapperExtension.AssociationMapperExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public class SubModelAttribute : Attribute
    {
    }
}
