using System;
using NewLibCore.InternalExtension;
using NewLibCore.Security;

namespace NewLibCore.Validate
{

    public static class Parameter
    {
        /// <summary>
        /// 验证引用类型是否合法
        /// </summary>
        public static void Validate(Object vaildateParameter, Boolean canNull = false)
        {
            if (!canNull && vaildateParameter == null)
            {
                throw new ArgumentNullException("参数不能为0或null");
            }
        }

        /// <summary>
        /// 验证值类型是否合法
        /// </summary>
        public static void Validate(ValueType valueType, Boolean canZero = false)
        {
            Type type = valueType.GetType();

            if (type.IsValueType && type.IsNumeric())
            {
                Boolean flag = !canZero ? valueType.CastTo(0.0) <= 0.0 : valueType.CastTo(0.0) < 0.0;

                if (flag)
                {
                    throw new ArgumentNullException("参数不能为0或null");
                }
            }
        }
    }
}
