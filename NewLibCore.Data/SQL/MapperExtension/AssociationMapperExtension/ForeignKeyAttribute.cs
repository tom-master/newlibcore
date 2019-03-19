using System;

namespace NewLibCore.Data.SQL.MapperExtension.AssociationMapperExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public class ForeignKeyAttribute : Attribute
    {
        public String MainTablePrimaryKey { get; private set; }

        public ForeignKeyAttribute(String primaryKey)
        {
            MainTablePrimaryKey = primaryKey;
        }
    }
}
