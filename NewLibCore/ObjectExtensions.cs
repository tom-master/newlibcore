using System;
using Microsoft.Extensions.Primitives;

namespace NewLibCore
{
    /// <summary>
    ///     通用类型扩展方法类
    /// </summary>
    public static class ObjectExtensions
    {
        /// <summary>
        /// 把对象类型转化为指定类型，转化失败时返回该类型默认值
        /// </summary>
        /// <typeparam name="T"> 动态类型 </typeparam>
        /// <param name="value"> 要转化的源对象 </param>
        /// <returns> 转化后的指定类型的对象，转化失败返回类型的默认值 </returns>
        public static T CastTo<T>(this Object value)
        {
            Object result;
            var type = typeof(T);
            try
            {
                if (type.IsEnum)
                {
                    if (Enum.TryParse(type, value.ToString(), false, out result))
                    {
                        return (T)result;
                    }
                    throw new ArgumentException($@"无法将值{value}转换为枚举类型:{type}");
                }
                if (type == typeof(Guid))
                {
                    if (Guid.TryParse(value.ToString(), out Guid guid))
                    {
                        return (T)guid;
                    }
                    result = Guid.Parse(value.ToString());
                }
                else
                {
                    result = Convert.ChangeType(value, type);
                }
            }
            catch
            {
                result = default(T);
            }

            return (T)result;
        }

        /// <summary>
        /// 把对象类型转化为指定类型，转化失败时返回指定的默认值
        /// </summary>
        /// <typeparam name="T"> 动态类型 </typeparam>
        /// <param name="value"> 要转化的源对象 </param>
        /// <param name="defaultValue"> 转化失败返回的指定默认值 </param>
        /// <returns> 转化后的指定类型对象，转化失败时返回指定的默认值 </returns>
        public static T CastTo<T>(this Object value, T defaultValue)
        {
            Object result;
            var type = typeof(T);
            try
            {
                result = type.IsEnum ? Enum.Parse(type, value.ToString()) : Convert.ChangeType(value, type);
            }
            catch
            {
                result = defaultValue;
            }
            return (T)result;
        }

        public static Object CastTo(this Object value, Type type)
        {
            Object result;
            try
            {
                result = type.IsEnum ? Enum.Parse(type, value.ToString()) : Convert.ChangeType(value, type);
            }
            catch
            {
                throw;
            }
            return result;
        }

        public static Boolean TryCastTo(this Object value, Type type)
        {
            try
            {
                if (type.IsEnum)
                {
                    Enum.Parse(type, value.ToString());
                }
                else if (type == typeof(Guid))
                {
                    Guid.Parse(value.ToString());
                }
                else
                {
                    Convert.ChangeType(value, type);
                }
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }
    }
}