using System;
using System.ComponentModel;
using System.Linq;
using NewLibCore.Validate;

namespace NewLibCore
{
    public static class EnumExtensions
    {
        /// <summary>
        /// 将输入的值转换成指定的枚举类型
        /// </summary>
        public static T ToEnum<T>(String value) where T : struct
        {
            Parameter.IfNullOrZero(value);

            if (String.IsNullOrEmpty(value))
            {
                throw new ArgumentException("value不能为空");
            }

            if (Enum.TryParse(value, true, out T t))
            {
                return t;
            }

            throw new ArgumentException($"{value}不是有效的类型");
        }

        /// <summary>
        /// 将输入的值转换成指定的枚举类型
        /// </summary>
        public static T ToEnum<T>(Int32 value) where T : struct
        {
            Parameter.IfNullOrZero(value); 
            return (T)Enum.ToObject(typeof(T), value);
        }

        /// <summary>
        /// 将输入的值转换成指定的枚举类型
        /// </summary>
        public static Int32 ToInt32(this Enum e)
        {
            Parameter.IfNullOrZero(e);
            return (Int32)Enum.Parse(e.GetType(), e.ToString());
        }

        /// <summary>
        /// 获取枚举的描述特性中的内容
        /// </summary>
        public static String GetDescription(this Enum e)
        {
            Parameter.IfNullOrZero(e);
            var attrs = e.GetType().GetField(e.ToString()).GetCustomAttributes(typeof(DescriptionAttribute), false);
            if (attrs.Length == 0)
            {
                throw new Exception("特性上没有用DescriptionAttribute修饰,因此无法获取描述");
            }
            return String.Join(",", attrs.Select(s => ((DescriptionAttribute)s).Description));
        }
    }
}
