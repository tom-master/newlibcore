using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Security;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data.Common;
using System.Data.SqlClient;

namespace NewLibCore.Data.SQL.DataStore
{
    public class SqlParameterMapper
    {
        public SqlParameterMapper(String key, Object value)
        {
            Key = key;
            Value = ParseValueType(value);
        }

        public String Key { get; private set; }

        public Object Value { get; private set; }

        public static implicit operator DbParameter(SqlParameterMapper value)
        {
            switch (SwitchDatabase.DatabaseType)
            {
                case DatabaseType.MSSQL:
                    return new SqlParameter(value.Key, value.Value);
                case DatabaseType.MYSQL:
                    return new MySqlParameter(value.Key, value.Value);
                default:
                    throw new ArgumentException($@"暂不支持的数据库类型:{SwitchDatabase.DatabaseType}");
            }
        }

        private Object ParseValueType(Object obj)
        {
            if (obj == null)
            {
                throw new ArgumentNullException($@"SQL参数:{Key}的值为null");
            }

            if (obj.GetType() == typeof(String))
            {
                return UnlegalChatDetection.FilterBadChat(obj.ToString());
            }

            var isComplexType = TypeDescriptor.GetConverter(obj.GetType()).CanConvertFrom(typeof(String));
            if (!isComplexType)
            {
                var objType = obj.GetType();
                if (objType.IsArray || objType.GetGenericTypeDefinition() == typeof(List<>))
                {
                    return String.Join(",", (IList<Int32>)obj);
                }
            }
            if (obj.GetType() == typeof(Boolean))
            {
                if ((Boolean)obj)
                {
                    return 1;
                }
                return 0;
            }

            return obj;
        }
    }
}
