using System;

namespace NewLibCore.Data.SQL.Mapper.AttributeExtension
{
    /// <summary>
    /// 表名
    /// </summary>
    public class TableNameAttribute : Attribute
    {
        public String TableName { get; private set; }

        
        public Boolean InvalidCacheThenUpdate { get; private set; }

        public TableNameAttribute(String name, Boolean invalidCacheThenUpdate) : this(name)
        {
            InvalidCacheThenUpdate = invalidCacheThenUpdate;
        }

        public TableNameAttribute(String name)
        {
            TableName = name;
        }
    }
}