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
    public sealed class MapperParameter
    {
        private readonly Boolean _filterBadContent;

        /// <summary>
        /// 初始化EntityParameter类的新实例
        /// </summary>
        /// <param name="key">占位符</param>
        /// <param name="value">值</param>
        /// <param name="filterBadContent">是否过滤非法字符</param>
        public MapperParameter(String key, Object value, Boolean filterBadContent)
        {
            Parameter.Validate(key);
            Parameter.Validate(value);

            _filterBadContent = filterBadContent;
            Key = $"@{key}";
            Value = ParseValueType(value);
        }

        /// <summary>
        /// 初始化EntityParameter类的新实例
        /// </summary>
        /// <param name="key">占位符</param>
        /// <param name="value">值</param>
        public MapperParameter(String key, Object value) : this(key, value, true)
        {

        }

        /// <summary>
        /// 占位符
        /// </summary>
        internal String Key { get; private set; }

        /// <summary>
        /// 值
        /// </summary>
        internal Object Value { get; private set; }

        public static implicit operator DbParameter(MapperParameter entityParameter)
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
                    if (_filterBadContent)
                    {
                        return UnlegalChatDetection.FilterBadChat(obj.ToString());
                    }
                    return obj.ToString();
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
