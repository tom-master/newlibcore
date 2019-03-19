using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]

    public class PrimaryKeyAttribute : Attribute
    {
    }
}
