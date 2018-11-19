using System;
using System.Collections.Generic;
using System.Reflection;

namespace NewLibCore.Data.SQL.PropertyExtension
{
    public abstract class PropertyMonitor
    {
        protected PropertyMonitor()
        {
            Args = new List<PropertyArgs>();
        }
        public IList<PropertyArgs> Args { get; }

        protected void OnPropertyChanged(params PropertyArgs[] propertyArgs)
        {
            if (propertyArgs.Length == 0)
            {
                return;
            }

            for (int i = 0; i < propertyArgs.Length; i++)
            {
                propertyArgs[i].SetPropertyInfo(GetType());
                Args.Add(propertyArgs[i]);
            }
        }

        public virtual void SetUpdateTime() { }
    }
}


