using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Data.SQL.Extension
{
    /// <summary>
    /// 将一个DataTable转换为指定的集合
    /// </summary>
    internal static class DataTableExtension
    {
        /// <summary>
        /// 获取列表
        /// </summary>
        /// <typeparam name="TResult">期望的类型</typeparam>
        /// <param name="dataTable">sql执行后的原始结果</param>
        /// <returns></returns>
        internal static List<TResult> ToList<TResult>(this DataTable dataTable)
        {
            if (dataTable == null || dataTable.Rows.Count == 0)
            {
                return new List<TResult>();
            }

            return InnerConvert<TResult>(dataTable);
        }

        private static List<TResult> InnerConvert<TResult>(DataTable dt)
        {
            try
            {
                var list = new List<TResult>();

                if (!typeof(TResult).IsComplexType())
                {
                    var obj = default(TResult);
                    if (typeof(TResult) != typeof(String))
                    {
                        obj = Activator.CreateInstance<TResult>();
                    }
                    var type = obj == null ? typeof(TResult) : obj.GetType();
                    for (var i = 0; i < dt.Rows.Count; i++)
                    {
                        list.Add((TResult)ChangeType(dt.Rows[i][0], type));
                    }
                }
                else if (typeof(TResult).Name.Contains("ValueTuple"))
                {
                    foreach (DataRow item in dt.Rows)
                    {
                        var r = CreateValueTuple(item.ItemArray);
                        list.Add((TResult)r);
                    }
                }
                else
                {

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
                                    fast.Set(obj, ChangeType(value, propertyInfo.PropertyType));
                                }
                            }
                        }
                        list.Add(obj);
                    }
                }

                return list;
            }
            catch (Exception)
            {
                throw;
            }
        }

        /// <summary>
        /// 创建一个值元组
        /// </summary>
        /// <param name="rowValues">原始查询结果中的行</param>
        /// <returns></returns>
        private static Object CreateValueTuple(Object[] rowValues)
        {
            if (rowValues.Length > 8)
            {
                throw new NotSupportedException($@"当已{nameof(ValueTuple)}为返回类型时,{nameof(ValueTuple)}中的项的个数与查询出的列的个数都不能大于8个");
            }

            var parameterTypes = new Type[rowValues.Length];
            for (var i = 0; i < rowValues.Length; i++)
            {
                parameterTypes[i] = rowValues[i].GetType();
            }

            var createMethod = typeof(ValueTuple)
            .GetMethods().Where(m => m.Name == "Create" && m.GetParameters().Length == rowValues.Length).SingleOrDefault();
            var createGenericMethod = createMethod.MakeGenericMethod(parameterTypes);
            var valueTuple = createGenericMethod.Invoke(null, rowValues);
            return valueTuple;
        }

        /// <summary>
        /// 修改目标值的类型
        /// </summary>
        /// <param name="value">目标值</param>
        /// <param name="type">转换类型</param>
        /// <returns></returns>
        private static Object ChangeType(Object value, Type type)
        {
            if (value == null)
            {
                return null;
            }

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
