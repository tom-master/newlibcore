using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public class ForeignKeyAttribute : Attribute
    {
    }
}
