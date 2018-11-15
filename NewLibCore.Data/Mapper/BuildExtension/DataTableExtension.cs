using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
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
                            var fast = new FastProperty(propertyInfo);
                            fast.Set(t, ConvertExtension.ChangeType(value, propertyInfo.PropertyType));
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

    public class FastProperty
    {
        public PropertyInfo Property { get; set; }

        public Func<object, object> GetDelegate;

        public Action<object, object> SetDelegate;

        public FastProperty(PropertyInfo property)
        {
            this.Property = property;
            InitializeGet();
            InitializeSet();
        }

        private void InitializeSet()
        {
            var instance = Expression.Parameter(typeof(object), "instance");
            var value = Expression.Parameter(typeof(object), "value");

            UnaryExpression instanceCast = (!this.Property.DeclaringType.IsValueType) ? Expression.TypeAs(instance, this.Property.DeclaringType) : Expression.Convert(instance, this.Property.DeclaringType);
            UnaryExpression valueCast = (!this.Property.PropertyType.IsValueType) ? Expression.TypeAs(value, this.Property.PropertyType) : Expression.Convert(value, this.Property.PropertyType);
            this.SetDelegate = Expression.Lambda<Action<object, object>>(Expression.Call(instanceCast, this.Property.SetMethod, valueCast), new ParameterExpression[] { instance, value }).Compile();
        }

        private void InitializeGet()
        {
            var instance = Expression.Parameter(typeof(object), "instance");
            UnaryExpression instanceCast = (!this.Property.DeclaringType.IsValueType) ? Expression.TypeAs(instance, this.Property.DeclaringType) : Expression.Convert(instance, this.Property.DeclaringType);
            this.GetDelegate = Expression.Lambda<Func<object, object>>(Expression.TypeAs(Expression.Call(instanceCast, this.Property.GetGetMethod()), typeof(object)), instance).Compile();
        }

        public object Get(object instance)
        {
            return this.GetDelegate(instance);
        }

        public void Set(object instance, object value)
        {
            this.SetDelegate(instance, value);
        }
    }
}
