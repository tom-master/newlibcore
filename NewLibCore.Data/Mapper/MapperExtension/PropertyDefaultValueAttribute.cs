using System;

namespace NewLibCore.Data.Mapper.MapperExtension
{
	public class PropertyDefaultValueAttribute : ValidateBase
	{
		private readonly Type _type;

		private readonly Object _value;

		public PropertyDefaultValueAttribute(Type type, Object value)
		{
			_type = type ?? throw new ArgumentException($@"{nameof(type)} 不能为空");
			_value = value ?? throw new ArgumentException($@"{nameof(value)} 不能为空");
		}

		public PropertyDefaultValueAttribute(Type type)
		{
			if (type == null)
			{
				throw new ArgumentException($@"{nameof(type)} 不能为空");
			}

			if (type.BaseType == typeof(ValueType))
			{
				_value = 0;
			}
			else
			{
				if (type == typeof(String))
				{
					_value = "";
				}
				else
				{
					_value = null;
				}
			}
			_type = type;
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
				return 1;
			}
		}

		public override String FailReason(String fieldName)
		{
			return $@"{fieldName} 的默认值类型转换失败";
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
