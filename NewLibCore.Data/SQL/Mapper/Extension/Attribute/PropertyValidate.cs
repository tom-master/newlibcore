using System;

namespace NewLibCore.Data.SQL.Mapper.Validate
{
    /// <summary>
    /// 基本的属性验证类
    /// </summary>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public abstract class PropertyValidate : Attribute
    {
        /// <summary>
        /// 是否验证通过
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public virtual Boolean IsValidate(Object value) { return false; }

        /// <summary>
        /// 特性生效的优先级
        /// </summary>
        /// <value></value>
        public virtual Int32 Order { get { return -1; } }

        /// <summary>
        /// 失败原因
        /// </summary>
        /// <param name="fieldName"></param>
        /// <returns></returns>
        public virtual String FailReason(String fieldName) { return fieldName; }
    }
}
