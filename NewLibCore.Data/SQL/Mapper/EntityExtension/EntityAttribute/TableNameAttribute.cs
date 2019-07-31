using System;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension.EntityAttribute
{
    /// <summary>
    /// 表名
    /// </summary>
    public class TableNameAttribute : Attribute
    {
        public String TableName { get; private set; }

        public String AliasName { get; private set; }

        public TableNameAttribute(String name, String aliasName = default)
        {
            TableName = name;
            AliasName = aliasName == default ? name : aliasName;
        }

    }
}