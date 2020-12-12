using System;
using NewLibCore.Security;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Validate
{
    /// <summary>
    /// 标记被修饰的类为数据库中的一个表
    /// </summary>
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = true)]
    public class TableNameAttribute : PropertyValidateAttribute
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
        public TableNameAttribute(String name, String aliasName = "")
        {
            Parameter.IfNullOrZero(name);

            TableName = BadChatDetection.FilterBadChat(name);
            if (String.IsNullOrEmpty(aliasName))
            {
                AliasName = name;
            }
            else
            {
                AliasName = BadChatDetection.FilterBadChat(aliasName);
            }
        }
    }
}