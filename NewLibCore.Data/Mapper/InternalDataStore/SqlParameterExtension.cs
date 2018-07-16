using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Data.SqlClient;
using System.Text;
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
