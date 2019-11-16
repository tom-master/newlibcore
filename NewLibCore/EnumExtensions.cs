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
            Parameter.Validate(value);

            if (String.IsNullOrEmpty(value))
            {
                throw new ArgumentException("value不能为空");
            }

            T t;
            if (Enum.TryParse(value, true, out t))
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
            Parameter.Validate(value);

            return (T)Enum.ToObject(typeof(T), value);
        }

        /// <summary>
        /// 将输入的值转换成指定的枚举类型
        /// </summary>
        public static Int32 ToInt32(this Enum e)
        {
            Parameter.Validate(e);
            return (Int32)Enum.Parse(e.GetType(), e.ToString());
        }

        /// <summary>
        /// 获取枚举的描述特性中的内容
        /// </summary>
        public static String GetDescription(this Enum e)
        {
            Parameter.Validate(e);
            var attrs = e.GetType().GetField(e.ToString()).GetCustomAttributes(typeof(DescriptionAttribute), false);
            if (attrs.Length == 0)
            {
                return "";
            }
            return String.Join(",", attrs.Select(s => ((DescriptionAttribute)s).Description));
        }
    }
}
