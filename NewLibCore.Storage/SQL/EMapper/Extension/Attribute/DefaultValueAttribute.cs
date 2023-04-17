﻿using System;
using System.ComponentModel.DataAnnotations;
using NewLibCore.Validate;
using StackExchange.Redis;

namespace NewLibCore.Storage.SQL.Validate
{
    /// <summary>
    /// 标记被修饰的属性有默认值
    /// </summary>
    public class DefaultValueAttribute : PropertyValidateAttribute
    {
        private ChangedProperty _changedProperty;

        /// <summary>
        /// 初始化一个DefaultValueAttribute对象实例
        /// </summary>
        /// <param name="value">默认值</param>
        public DefaultValueAttribute(Object value) : this(value.GetType(), value)
        {

        }

        /// <summary>
        /// 初始化一个DefaultValueAttribute对象实例
        /// </summary>
        /// <param name="type">默认值类型</param>
        /// <param name="value">默认值</param>
        public DefaultValueAttribute(Type type, Object value)
        {
            Check.IfNullOrZero(type);

            Type = type;
            if (type.IsEnum)
            {
                if (value == null)
                {
                    throw new InvalidCastException($@"枚举类型 {type} 的默认值必须被手动指定");
                }
            }

            if (!type.IsComplexType())
            {
                var hasNullOrEmpty = string.IsNullOrEmpty(value + "");
                if (type == typeof(bool))
                {
                    Value = !hasNullOrEmpty && value.CastTo((bool)value);
                }
                else if (type == typeof(Guid))
                {
                    Value = hasNullOrEmpty ? Guid.NewGuid() : value.CastTo((Guid)value);
                }
                else if (type.IsNumeric())
                {
                    Value = hasNullOrEmpty ? 0 : value.CastTo((int)value);
                }
                else if (type == typeof(string))
                {
                    Value = hasNullOrEmpty ? "" : value.CastTo(value.ToString());
                }
                else if (type == typeof(DateTime?) || type == typeof(DateTime))
                {
                    Value = hasNullOrEmpty ? DateTime.Now : value.CastTo(DateTime.Parse(value.ToString()));
                }
            }
            else
            {
                throw new InvalidCastException("暂不支持默认值为复杂类型的转换");
            }
        }

        /// <summary>
        /// 初始化一个DefaultValueAttribute类的实例
        /// </summary>
        /// <param name="type">默认值类型</param>
        public DefaultValueAttribute(Type type) : this(type, default)
        {

        }

        /// <summary>
        /// 默认值
        /// </summary>
        public Object Value { get; private set; }

        public Type Type { get; private set; }

        internal override int Order => 2;

        internal override string FailReason(string fieldName)
        {
            return $@"{fieldName} 的默认值从{Value}类型转为{_changedProperty.Type.Name}时失败";
        }

        internal override bool IsValidate(ChangedProperty property)
        {
            _changedProperty = property;

            //判断DefaultValueAttribute中传入的Type与EntityBase子类属性中的数据的实际类型
            //防止在DateTime类型的属性上使用DefaultValueAttribute向内传入不能转换成DateTime的数据，包括类似于"0000/00/01 00:00:00","WASD","123123"等无法转换为DateTime的数据
            if (property.Type != Type)
            {
                return false;
            }

            try
            {
                Convert.ChangeType(Value, property.Type);
                return true;
            }
            catch (System.Exception)
            {
                return false;
            }
        }
    }
}
