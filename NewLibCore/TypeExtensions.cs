using System;
using System.Collections;
using System.ComponentModel;
using System.Linq;
using System.Reflection;
using NewLibCore.Validate;

namespace NewLibCore
{
    /// <summary>
    ///     类型扩展方法类
    /// </summary>
    public static class TypeExtensions
    {
        /// <summary>
        /// 判断指定类型是否为值类型
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static Boolean IsNumeric(this Type type)
        {
            return type == typeof(Byte)
                || type == typeof(Int16)
                || type == typeof(Int32)
                || type == typeof(Int64)
                || type == typeof(SByte)
                || type == typeof(UInt16)
                || type == typeof(UInt32)
                || type == typeof(UInt64)
                || type == typeof(Decimal)
                || type == typeof(Double)
                || type == typeof(Single) || type.IsEnum;
        }

        public static String ToDescription(this MemberInfo member, Boolean inherit = false)
        {
            var desc = member.GetAttribute<DescriptionAttribute>(inherit);
            return desc?.Description;
        }

        public static Boolean AttributeExists<T>(this MemberInfo memberInfo, Boolean inherit = false) where T : Attribute
        {
            return memberInfo.GetCustomAttributes(typeof(T), inherit).Any(m => (m as T) != null);
        }

        public static T GetAttribute<T>(this MemberInfo memberInfo, Boolean inherit = false) where T : Attribute
        {
            return memberInfo.GetCustomAttributes(typeof(T), inherit).SingleOrDefault() as T;
        }

        public static T[] GetAttributes<T>(this MemberInfo memberInfo, Boolean inherit = false) where T : Attribute
        {
            return memberInfo.GetCustomAttributes(typeof(T), inherit).Cast<T>().ToArray();
        }

        public static Boolean AttributeExists<T>(this PropertyInfo propertyInfo, Boolean inherit = false) where T : Attribute
        {
            return propertyInfo.GetCustomAttributes(typeof(T), inherit).Any(m => (m as T) != null);
        }

        public static T GetAttribute<T>(this PropertyInfo propertyInfo, Boolean inherit = false) where T : Attribute
        {
            return propertyInfo.GetCustomAttributes(typeof(T), inherit).SingleOrDefault() as T;
        }

        public static T[] GetAttributes<T>(this PropertyInfo propertyInfo, Boolean inherit = false) where T : Attribute
        {
            return propertyInfo.GetCustomAttributes(typeof(T), inherit).Cast<T>().ToArray();
        }



        /// <summary>
        /// 判断是否为复杂类型
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static Boolean IsComplexType(this Type type)
        {
            return !TypeDescriptor.GetConverter(type).CanConvertFrom(typeof(String));
        }

        public static Boolean IsCollection(this Type type)
        {
            var interfaces = type.GetInterfaces();
            return interfaces.Any(w => w == typeof(IEnumerable)) || interfaces.Any(w => w == typeof(ICollection)) || interfaces.Any(w => w == typeof(IList));
        }
    }
}