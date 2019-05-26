using System;

namespace NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension
{
    public class TableNameAttribute : Attribute
    {
        public String TableName { get; private set; }

        public TableNameAttribute(String name)
        {
            TableName = name;
        }
    }
}