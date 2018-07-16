using System;
using System.Collections.Generic;

namespace NewLibCore.Data.Mapper.PropertyExtension
{
	public abstract class PropertyMonitor
	{
		protected PropertyMonitor()
		{
			Args = new List<PropertyArgs>();
		}

		protected void OnPropertyChanged(params PropertyArgs[] propertyNames)
		{
			if (propertyNames.Length == 0)
			{
				return;
			}

			var currentTypeFullName = GetType().FullName;

			for (int i = 0; i < propertyNames.Length; i++)
			{
				var instanceName = $@"{currentTypeFullName}.{propertyNames[i].PropertyName}";
				Args.Add(new PropertyArgs(propertyNames[i].PropertyName, propertyNames[i].PropertyValue));
			}
		}

		public IList<PropertyArgs> Args { get; private set; }
	}
}

public class PropertyArgs
{
	internal String PropertyName { get; }

	public Object PropertyValue { get; }

	public PropertyArgs(String propertyName, Object propertyValue)
	{
		PropertyName = propertyName;
		PropertyValue = propertyValue;
	}

	public String GetArgumentName()
	{
		var lastIndex = PropertyName.LastIndexOf(".");
		return PropertyName.Substring(lastIndex + 1);
	}
}

