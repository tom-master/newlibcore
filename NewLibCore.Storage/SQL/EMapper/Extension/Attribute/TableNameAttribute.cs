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
        public string TableName { get; private set; }

        /// <summary>
        /// 表别名
        /// </summary>
        public string AliasName { get; private set; }

        /// <summary>
        /// 初始化一个TableName的实例
        /// </summary>
        /// <param name="name">表名</param>
        /// <param name="aliasName">表别名</param>
        public TableNameAttribute(string name, string aliasName = "")
        {
            Check.IfNullOrZero(name);

            TableName = BadChatDetection.FilterBadChat(name);
            if (string.IsNullOrEmpty(aliasName))
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