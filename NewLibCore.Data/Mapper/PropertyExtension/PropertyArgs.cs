using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;

namespace NewLibCore.Data.Mapper.PropertyExtension
{
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

		internal void SetPropertyInfo(Type modelType)
		{
			PropertyInfo = modelType.GetProperty(PropertyName);
		}
	}
}
