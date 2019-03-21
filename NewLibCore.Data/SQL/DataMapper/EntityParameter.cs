using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Security;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data.Common;
using System.Linq;

namespace NewLibCore.Data.SQL.DataMapper
{
    public class EntityParameter
    {
        public EntityParameter(String key, Object value)
        {
            Key = key;
            Value = ParseValueType(value);
        }

        public String Key { get; private set; }

        public Object Value { get; private set; }

        public static implicit operator DbParameter(EntityParameter entityParameter)
        {
            var parameter = DatabaseConfig.GetDbParameterInstance();
            parameter.ParameterName = entityParameter.Key;
            parameter.Value = entityParameter.Value;
            return parameter;
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
                    if (objType.GetGenericArguments()[0] == typeof(String))
                    {
                        return String.Join(",", ((IList<String>)obj).Select(s => $@"'{s}'"));
                    }
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
