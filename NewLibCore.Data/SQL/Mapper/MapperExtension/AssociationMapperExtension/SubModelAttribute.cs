using System;

namespace NewLibCore.Data.SQL.Mapper.Extension.AssociationMapperExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public class SubModelAttribute : Attribute
    {
    }
}
