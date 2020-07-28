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

        public static Boolean AttributeExists<T>(this MemberInfo memberInfo, Boolean inherit) where T : Attribute
        {
            return memberInfo.GetCustomAttributes(typeof(T), inherit).Any(m => (m as T) != null);
        }

        public static T GetAttribute<T>(this MemberInfo memberInfo, Boolean inherit) where T : Attribute
        {
            return memberInfo.GetCustomAttributes(typeof(T), inherit).SingleOrDefault() as T;
        }

        public static T[] GetAttributes<T>(this MemberInfo memberInfo, Boolean inherit) where T : Attribute
        {
            return memberInfo.GetCustomAttributes(typeof(T), inherit).Cast<T>().ToArray();
        }

        public static Boolean IsAssignableToGenericType(this Type givenType, Type genericType)
        {
            if (!genericType.IsGenericType)
            {
                return false;
            }
            var interfaceTypes = givenType.GetInterfaces();
            if (interfaceTypes.Any(interfaceType => interfaceType.IsGenericType && interfaceType.GetGenericTypeDefinition() == genericType))
            {
                return true;
            }
            if (givenType.IsGenericType && givenType.GetGenericTypeDefinition() == genericType)
            {
                return true;
            }
            var baseType = givenType.BaseType;
            if (baseType == null)
            {
                return false;
            }
            return IsAssignableToGenericType(baseType, genericType);
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

        /// <summary>
        /// 修改目标值的类型
        /// </summary>
        /// <param name="value">目标值</param>
        /// <param name="type">转换类型</param>
        /// <returns></returns>
        public static Object ChangeType(this Object value, Type type)
        {
            Parameter.Validate(value);
            Parameter.Validate(type);

            if (typeof(Enum).IsAssignableFrom(type))
            {
                return Enum.Parse(type, value.ToString());
            }
            return Convert.ChangeType(value, type);
        }
    }
}