using System;
using System.ComponentModel;
using System.Linq;

namespace NewLibCore
{
    public static class EnumExtensions
    {
        public static T ToEnum<T>(String value) where T : struct
        {
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

        public static T ToEnum<T>(Int32 value) where T : struct
        {
            return (T)Enum.ToObject(typeof(T), value);
        }

        public static Int32 ToInt32(this Enum e)
        {
            return (Int32)Enum.Parse(e.GetType(), e.ToString());
        }

        public static String GetDescription(this Enum e)
        {
            var attrs = e.GetType().GetField(e.ToString()).GetCustomAttributes(typeof(DescriptionAttribute), false);
            if (attrs.Length == 0)
            {
                return "";
            }
            return String.Join(",", attrs.ToList().Select(s => ((DescriptionAttribute)s).Description));
        }
    }
}
