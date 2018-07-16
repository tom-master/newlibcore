using System;

namespace NewLibCore
{
	public sealed class EnumExtensions
    {
        public static T ToEnum<T>(String value) where T : struct
        {
            T t;
            if (Enum.TryParse(value, true, out t))
            {
                return t;
            }

            throw new ArgumentException($"{value}不是有效的类型");
        }

        /// <summary>
        /// 参数转换为枚举类型
        /// </summary>
        public static T ToEnum<T>(Int32 value) where T : struct
        {
            return (T)Enum.ToObject(typeof(T), value);
        }
    }
}
