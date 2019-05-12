using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    internal static class DataTableExtension
    {
        /// <summary>
        /// 获取列表
        /// </summary>
        internal static List<T> ToList<T>(this DataTable dataTable) where T : new()
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
        internal static T ToSingle<T>(this DataTable dataTable) where T : new()
        {
            return ToList<T>(dataTable).FirstOrDefault();
        }

        private static List<T> ConvertToList<T>(DataTable dt) where T : new()
        {
            var list = new List<T>();
            foreach (DataRow dr in dt.Rows)
            {
                var t = new T();
                var propertys = t.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public);
                foreach (var propertyInfo in propertys)
                {
                    var tempName = propertyInfo.Name;
                    if (dt.Columns.Contains(tempName))
                    {
                        var value = dr[tempName];
                        if (value != DBNull.Value)
                        {
                            try
                            {
                                var fast = new FastProperty(propertyInfo);
                                fast.Set(t, ConvertExtension.ChangeType(value, propertyInfo.PropertyType));
                            }
                            catch (Exception)
                            {
                                throw;
                            }
                        }
                    }
                }
                list.Add(t);
            }
            return list;
        }
    }

    internal static class ConvertExtension
    {
        internal static Object ChangeType(Object value, Type type)
        {
            if (typeof(Enum).IsAssignableFrom(type))
            {
                return Enum.Parse(type, value.ToString());
            }
            return Convert.ChangeType(value, type);
        }
    }

    internal class FastProperty
    {
        public PropertyInfo Property { get; set; }

        public Func<Object, Object> GetDelegate;

        public Action<Object, Object> SetDelegate;

        public FastProperty(PropertyInfo property)
        {
            Property = property;
            InitializeGet();
            InitializeSet();
        }

        private void InitializeSet()
        {
            var instance = Expression.Parameter(typeof(Object), "instance");
            var value = Expression.Parameter(typeof(Object), "value");

            var instanceCast = (!Property.DeclaringType.IsValueType) ? Expression.TypeAs(instance, Property.DeclaringType) : Expression.Convert(instance, Property.DeclaringType);
            var valueCast = (!Property.PropertyType.IsValueType) ? Expression.TypeAs(value, Property.PropertyType) : Expression.Convert(value, Property.PropertyType);
            SetDelegate = Expression.Lambda<Action<Object, Object>>(Expression.Call(instanceCast, Property.SetMethod, valueCast), new ParameterExpression[] { instance, value }).Compile();
        }

        private void InitializeGet()
        {
            var instance = Expression.Parameter(typeof(Object), "instance");
            var instanceCast = (!Property.DeclaringType.IsValueType) ? Expression.TypeAs(instance, Property.DeclaringType) : Expression.Convert(instance, Property.DeclaringType);
            GetDelegate = Expression.Lambda<Func<Object, Object>>(Expression.TypeAs(Expression.Call(instanceCast, Property.GetGetMethod()), typeof(Object)), instance).Compile();
        }

        internal Object Get(Object instance)
        {
            return GetDelegate(instance);
        }

        internal void Set(Object instance, Object value)
        {
            SetDelegate(instance, value);
        }
    }
}
