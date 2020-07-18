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
            if (Enum.TryParse(value, true, out T t))
            {
                return t;
            }

            throw new ArgumentException($"{value}不是有效的值");
        }

        /// <summary>
        /// 将输入的值转换成指定的枚举类型
        /// </summary>
        public static T ToEnum<T>(Int32 value) where T : struct
        {
            Parameter.IfNullOrZero(value);
            return ToEnum<T>(value.ToString());
        }

        /// <summary>
        /// 将输入的值转换成指定的枚举类型
        /// </summary>
        public static Int32 ToInt32(this Enum e)
        {
            Parameter.IfNullOrZero(e);

            if (Enum.TryParse(e.GetType(), e.ToString(), true, out Object c))
            {
                return (Int32)c;
            }

            throw new ArgumentException($@"无效的枚举值{e}");
        }

        /// <summary>
        /// 获取枚举的描述特性中的内容
        /// </summary>
        public static String GetDescription(this Enum e)
        {
            Parameter.IfNullOrZero(e);
            var attrs = e.GetType().GetField(e.ToString()).GetAttributes<DescriptionAttribute>(false);
            if (attrs.Length > 0)
            {
                return String.Join(",", attrs.Select(s => s.Description));
            }
            throw new ArgumentException($@"枚举值:{e}没有应用DescriptionAttribute特性,因此无法获取描述");
        }
    }
}
