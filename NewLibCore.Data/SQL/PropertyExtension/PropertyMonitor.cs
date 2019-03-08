using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Collections.Generic;
using System.Reflection;

namespace NewLibCore.Data.SQL.PropertyExtension
{
    public abstract class PropertyMonitor
    {
        [Ignore]
        public IList<PropertyInfo> PropertyInfos { get; }

        protected PropertyMonitor()
        {
            PropertyInfos = new List<PropertyInfo>();
        }

        protected void OnPropertyChanged(String propertyName)
        {
            if (String.IsNullOrEmpty(propertyName))
            {
                throw new ArgumentNullException("需要指定更新的字段名称");
            }
            var propertyInfo = GetType().GetProperty(propertyName);
            if (propertyInfo == null)
            {
                throw new ArgumentException($@"属性：{propertyName},不属于类：{GetType().Name}");
            }
            PropertyInfos.Add(propertyInfo);
        }

        public virtual void SetUpdateTime() { }
    }
}


