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
	}
}

public class PropertyArgs
{
	internal String PropertyName { get; }

	internal PropertyInfo PropertyInfo { get; private set; }

	private Object _propertyValue { get; }

	public PropertyArgs(String propertyName, Object propertyValue)
	{
		if (String.IsNullOrEmpty(propertyName) || propertyValue == null)
		{
			throw new Exception();
		}

		PropertyName = propertyName;
		_propertyValue = propertyValue;
	}

	internal void SetPropertyInfo(Type propertyType)
	{
		PropertyInfo = propertyType.GetProperty(PropertyName);
	}

	internal Object GetPropertyValue(Object modelInstance)
	{
		return PropertyInfo.GetValue(modelInstance);
	}
}

