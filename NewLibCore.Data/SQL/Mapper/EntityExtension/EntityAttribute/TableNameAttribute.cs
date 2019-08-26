using System;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    /// <summary>
    /// 标记被修饰的类为数据库中的一个表
    /// </summary>
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = false)]
    public class TableNameAttribute : Attribute
    {
        /// <summary>
        /// 表名
        /// </summary>
        public String TableName { get; private set; }

        /// <summary>
        /// 表别名
        /// </summary>
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