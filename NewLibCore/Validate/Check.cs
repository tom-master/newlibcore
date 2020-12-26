using System;

namespace NewLibCore.Validate
{
    public static class Check
    {
        /// <summary>
        /// 是否为0或null
        /// </summary>
        public static void IfNullOrZero(Object argument)
        {
            IfNullOrZero(argument, false);
        }

        /// <summary>
        /// 是否为0或null
        /// </summary>
        public static void IfNullOrZero(Object argument, Boolean canNull)
        {
            if (!canNull && (argument == null || String.IsNullOrEmpty(argument.ToString())))
            {
                throw new ArgumentNullException("参数不能为0或null");
            }
        }

        /// <summary>
        /// 是否为0或null
        /// </summary>
        /// <param name="valueType"></param>
        public static void IfNullOrZero(ValueType valueType)
        {
            IfNullOrZero(valueType, false);
        }

        /// <summary>
        /// 是否为0或null
        /// </summary>
        public static void IfNullOrZero(ValueType valueType, Boolean canZero)
        {
            var type = valueType.GetType();

            if (type.IsValueType && type.IsNumeric())
            {
                var flag = !canZero ? valueType.CastTo(0.0) <= 0.0 : valueType.CastTo(0.0) < 0.0;

                if (flag)
                {
                    throw new ArgumentNullException("参数不能为0或null");
                }
            }
        }
    }
}
