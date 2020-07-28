using System;
using System.Collections.Generic;
using System.Data;
using System.Reflection;
using System.Text;

namespace NewLibCore.Data.SQL.DataConvert
{
    internal class ClassConverter : IConverter
    {
        public List<TResult> Convert<TResult>(DataTable dt)
        {
            var convertResults = new List<TResult>();
            IList<PropertyInfo> propertiesCache = null;
            foreach (DataRow item in dt.Rows)
            {
                var obj = Activator.CreateInstance<TResult>();
                if (propertiesCache == null)
                {
                    propertiesCache = obj.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public);
                }

                foreach (var propertyInfo in propertiesCache)
                {
                    if (dt.Columns.Contains(propertyInfo.Name))
                    {
                        var value = item[propertyInfo.Name];
                        if (value != DBNull.Value)
                        {
                            var fast = new FastProperty(propertyInfo);
                            fast.Set(obj, value.ChangeType(propertyInfo.PropertyType));
                        }
                    }
                }
                convertResults.Add(obj);
            }
            return convertResults;
        }
    }
}
