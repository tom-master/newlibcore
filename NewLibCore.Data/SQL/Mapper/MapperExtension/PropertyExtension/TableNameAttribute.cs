using System;

namespace NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension
{
    public class TableNameAttribute : Attribute
    {
        public String TableName { get; private set; }

        /// <summary>
        /// 表数据变更时讲缓存失效
        /// </summary>
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