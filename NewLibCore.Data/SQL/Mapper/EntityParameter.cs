using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Logger;
using NewLibCore.Security;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 实体参数
    /// </summary>
    public sealed class EntityParameter
    {
        public EntityParameter(String key, Object value)
        {
            Parameter.Validate(key);
            Parameter.Validate(value);

            Key = $"@{key}";
            Value = ParseValueType(value);
        }

        internal String Key { get; private set; }

        internal Object Value { get; private set; }

        public static implicit operator DbParameter(EntityParameter entityParameter)
        {
            Parameter.Validate(entityParameter);

            var instance = MapperConfig.ServiceProvider.GetService<InstanceConfig>();

            var parameter = instance.GetParameterInstance();
            parameter.ParameterName = entityParameter.Key;
            parameter.Value = entityParameter.Value;
            return parameter;
        }

        /// <summary>
        /// 转换传入的数据类型
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        private Object ParseValueType(Object obj)
        {
            try
            {
                Parameter.Validate(obj);
                var objType = obj.GetType();
                if (objType == typeof(String))
                {
                    return UnlegalChatDetection.FilterBadChat(obj.ToString());
                }

                if (objType == typeof(Boolean))
                {
                    return (Boolean)obj ? 1 : 0;
                }

                if (objType.IsComplexType())
                {
                    if (objType.IsArray || objType.IsCollections())
                    {
                        var argument = objType.GetGenericArguments();
                        if (argument.Any() && argument[0] == typeof(String))
                        {
                            return String.Join(",", ((IList<String>)obj).Select(s => $@"'{s}'"));
                        }
                        if (argument.Any() && argument[0].IsNumeric())
                        {
                            return String.Join(",", (IList<Int32>)obj);
                        }
                        if (argument.Any() && argument[0] == typeof(DateTime))
                        {
                            return String.Join(",", (IList<DateTime>)obj);
                        }
                    }
                    var ex = $@"无法转换的类型{objType.Name}";
                    MapperConfig.ServiceProvider.GetService<ILogger>().Error(ex);
                    throw new Exception(ex);
                }
                return obj;
            }
            catch (Exception ex)
            {
                MapperConfig.ServiceProvider.GetService<ILogger>().Error(ex.ToString());
                throw;
            }
        }
    }
}
