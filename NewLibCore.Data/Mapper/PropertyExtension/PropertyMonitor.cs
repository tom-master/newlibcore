using System;
using System.Collections.Generic;
using System.Reflection;

namespace NewLibCore.Data.Mapper.PropertyExtension
{
	public abstract class PropertyMonitor
	{
		protected PropertyMonitor()
		{
			Args = new List<PropertyArgs>();
		}
		public IList<PropertyArgs> Args { get; }

		protected void OnPropertyChanged(params PropertyArgs[] propertyNames)
		{
			if (propertyNames.Length == 0)
			{
				return;
			}

			for (int i = 0; i < propertyNames.Length; i++)
			{
				propertyNames[i].SetPropertyInfo(GetType());
				Args.Add(propertyNames[i]);
			}
		}
	}
}

public class PropertyArgs
{
	internal String PropertyName { get; }

	internal PropertyInfo PropertyInfo { get; private set; }

	internal Object PropertyValue { get; }

	public PropertyArgs(String propertyName, Object propertyValue)
	{
		if (String.IsNullOrEmpty(propertyName) || propertyValue == null)
		{
			throw new Exception();
		}

		PropertyName = propertyName;
		PropertyValue = propertyValue;
	}


	internal void SetPropertyInfo(Type propertyType)
	{
		PropertyInfo = propertyType.GetProperty(PropertyName);
	}
}

