using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data.Common;
using MySql.Data.MySqlClient;

namespace NewLibCore.Data.Mapper.InternalDataStore
{
	public class ParameterMapper
	{
		public ParameterMapper(String key, Object value)
		{
			Key = key;
			Value = ParseValueType(value);
		}

		public String Key { get; private set; }

		public Object Value { get; private set; }

		public static implicit operator DbParameter(ParameterMapper value)
		{
			return new MySqlParameter(value.Key, value.Value);
		}

		private Object ParseValueType(Object obj)
		{
			var isComplexType = TypeDescriptor.GetConverter(obj.GetType()).CanConvertFrom(typeof(String));
			if (!isComplexType)
			{
				if (obj.GetType().GetGenericTypeDefinition() == typeof(List<>))
				{
					return String.Join(",", (IList<Int32>)obj);
				}
			}
			if (obj.GetType() == typeof(Boolean))
			{
				if (((Boolean)obj))
				{
					return 1;
				}
				else
				{
					return 0;
				}
			}
			
			return obj;
		}
	}
}
