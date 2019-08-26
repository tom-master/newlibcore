using System;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    /// <summary>
    /// 表名
    /// </summary>
    public class TableNameAttribute : Attribute
    {
        public String TableName { get; private set; }

        public String AliasName { get; private set; }

        /// <summary>
        /// 初始化一个TableName的实例
        /// </summary>
        /// <param name="name">表名</param>
        /// <param name="aliasName">表别名</param>
        public TableNameAttribute(String name, String aliasName = default)
        {
            TableName = name;
            AliasName = aliasName == default ? name : aliasName;
        }

    }
}