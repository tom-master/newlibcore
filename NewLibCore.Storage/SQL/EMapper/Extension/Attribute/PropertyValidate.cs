using System;

namespace NewLibCore.Storage.SQL.Validate
{
    /// <summary>
    /// 基本的属性验证类
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
    public abstract class PropertyValidateAttribute : Attribute
    {
        /// <summary>
        /// 是否验证通过
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        internal virtual Boolean IsValidate(ChangedProperty property) { return false; }

        /// <summary>
        /// 特性生效的优先级
        /// </summary>
        /// <value></value>
        internal virtual Int32 Order { get { return -1; } }

        /// <summary>
        /// 失败原因
        /// </summary>
        /// <param name="fieldName"></param>
        /// <returns></returns>
        internal virtual String FailReason(String fieldName) { return fieldName; }
    }
}
