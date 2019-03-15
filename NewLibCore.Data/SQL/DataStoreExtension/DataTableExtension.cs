﻿using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Data.SQL.DataExtension
{
    internal static class DataTableExtension
    {
        /// <summary>
        /// 获取列表
        /// </summary>
        internal static IList<T> AsList<T>(this DataTable dataTable) where T : new()
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
        internal static T AsSignal<T>(this DataTable dataTable) where T : new()
        {
            return AsList<T>(dataTable).FirstOrDefault();
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
            this.Property = property;
            InitializeGet();
            InitializeSet();
        }

        private void InitializeSet()
        {
            var instance = Expression.Parameter(typeof(Object), "instance");
            var value = Expression.Parameter(typeof(Object), "value");

            var instanceCast = (!this.Property.DeclaringType.IsValueType) ? Expression.TypeAs(instance, this.Property.DeclaringType) : Expression.Convert(instance, this.Property.DeclaringType);
            var valueCast = (!this.Property.PropertyType.IsValueType) ? Expression.TypeAs(value, this.Property.PropertyType) : Expression.Convert(value, this.Property.PropertyType);
            this.SetDelegate = Expression.Lambda<Action<Object, Object>>(Expression.Call(instanceCast, this.Property.SetMethod, valueCast), new ParameterExpression[] { instance, value }).Compile();
        }

        private void InitializeGet()
        {
            var instance = Expression.Parameter(typeof(Object), "instance");
            var instanceCast = (!this.Property.DeclaringType.IsValueType) ? Expression.TypeAs(instance, this.Property.DeclaringType) : Expression.Convert(instance, this.Property.DeclaringType);
            this.GetDelegate = Expression.Lambda<Func<Object, Object>>(Expression.TypeAs(Expression.Call(instanceCast, this.Property.GetGetMethod()), typeof(Object)), instance).Compile();
        }

        internal Object Get(Object instance)
        {
            return this.GetDelegate(instance);
        }

        internal void Set(Object instance, Object value)
        {
            this.SetDelegate(instance, value);
        }
    }
}