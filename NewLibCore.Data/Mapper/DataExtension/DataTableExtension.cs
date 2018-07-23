using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.Mapper.DataExtension
{
	public static class DataTableExtension
	{
		/// <summary>
		/// 获取列表
		/// </summary>
		public static IList<T> AsList<T>(this DataTable dataTable) where T : class, new()
		{
			if (dataTable == null || dataTable.Rows.Count == 0)
			{
				return new List<T>();
			}

			return ConvertToList<T>(dataTable);
		}

		/// <summary>
		/// 获取单值
		/// </summary>
		public static T AsSignal<T>(this DataTable dataTable) where T : class, new()
		{
			return AsList<T>(dataTable).FirstOrDefault();
		}

		private static List<T> ConvertToList<T>(DataTable dt) where T : class, new()
		{
			var list = new List<T>();
			foreach (DataRow dr in dt.Rows)
			{
				var t = new T();
				PropertyInfo[] propertys = t.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public);
				foreach (PropertyInfo propertyInfo in propertys)
				{
					var tempName = propertyInfo.Name;
					if (dt.Columns.Contains(tempName))
					{
						var value = dr[tempName];
						if (value != DBNull.Value)
						{
							propertyInfo.SetValue(t, ConvertExtension.ChangeType(value, propertyInfo.PropertyType), null);
						}
					}
				}
				list.Add(t);
			}
			return list;
		}
	}

	public static class ConvertExtension
	{
		public static Object ChangeType(Object value, Type type)
		{
			if (typeof(Enum).IsAssignableFrom(type))
			{
				return Enum.Parse(type, value.ToString());
			}
			return Convert.ChangeType(value, type);
		}
	}
}
