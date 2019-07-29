using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Config;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    /// <summary>
    /// 将一个DataTable转换为指定的集合
    /// </summary>
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
            var tableName = $@"{typeof(T).GetAliasName()}_";
            foreach (DataRow dr in dt.Rows)
            {
                var masterTable = Activator.CreateInstance<T>();
                var propertys = masterTable.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public);
                foreach (var propertyInfo in propertys)
                {
                    var propertyName = $@"{tableName}{propertyInfo.Name}";
                    if (dt.Columns.Contains(propertyName))
                    {
                        var value = dr[propertyName];
                        if (value != DBNull.Value)
                        {
                            try
                            {
                                var fast = new FastProperty(propertyInfo);
                                fast.Set(masterTable, ConvertExtension.ChangeType(value, propertyInfo.PropertyType));
                            }
                            catch (Exception ex)
                            {
                                MapperConfig.DatabaseConfig.Logger.Error($@"{typeof(T).Name}转换失败:{ex}");
                            }
                        }
                    }
                }
                list.Add(masterTable);
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
