using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Data.SqlClient;
using System.Linq;
using MySql.Data.MySqlClient;
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



            DbParameter parameter = null;
            if (MapperConfig.MapperType == MapperType.MSSQL)
            {
                parameter = new SqlParameter();
            }
            else if (MapperConfig.MapperType == MapperType.MYSQL)
            {
                parameter = new MySqlParameter();
            }
            else
            {
                throw new Exception($@"暂不支持的数据库类型:{MapperConfig.MapperType}");
            }

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
                    MapperConfig.Logger.Error(ex);
                    throw new Exception(ex);
                }
                return obj;
            }
            catch (Exception ex)
            {
                MapperConfig.Logger.Error(ex.ToString());
                throw;
            }
        }
    }
}
