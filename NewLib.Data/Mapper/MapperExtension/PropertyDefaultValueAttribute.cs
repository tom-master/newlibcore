using System;

namespace NewLib.Data.Mapper.MapperExtension
{
	public class PropertyDefaultValueAttribute: ValidateBase
	{
		private Type _type;

		private Object _value;

		public PropertyDefaultValueAttribute(Type type, Object value)
		{
			if (type == null)
			{
				throw new ArgumentException($@"{nameof(type)} is null");
			}

			if (value == null)
			{
				throw new ArgumentException($@"{nameof(value)} is null");
			}

			_type = type;
			_value = value;
		}

		public Type Type
		{
			get
			{
				return _type;
			}
		}

		public Object Value
		{
			get
			{
				return _value;
			}
		}

		public override Int32 Order
		{
			get
			{
				return 2;
			}
		}

		public override String FailReason(String fieldName)
		{
			return $@"{fieldName}的默认值类型转换失败";
		}

		public override bool IsValidate(object value)
		{
			if (value == null)
			{
				value = _value;
			}

			try
			{
				Convert.ChangeType(value, _type);
				return true;
			}
			catch (Exception)
			{
				return false;
			}
		}
	}
}
