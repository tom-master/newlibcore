using System;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Validate
{
	/// <summary>
	/// 标记被修饰的属性有默认值
	/// </summary>
	public class DefaultValueAttribute : PropertyValidate
	{
		private ChangedProperty _changedProperty;

		/// <summary>
		/// 初始化一个DefaultValueAttribute对象实例
		/// </summary>
		/// <param name="value">默认值</param>
		public DefaultValueAttribute(Object value) : this(value.GetType(), value)
		{

		}

		/// <summary>
		/// 初始化一个DefaultValueAttribute对象实例
		/// </summary>
		/// <param name="type">默认值类型</param>
		/// <param name="value">默认值</param>
		public DefaultValueAttribute(Type type, Object value)
		{
			Parameter.Validate(type);

			if (type.BaseType == typeof(Enum))
			{
				if (value == null)
				{
					throw new ArgumentException($@"枚举类型 {type.ToString()} 的默认值必须被手动指定");
				}
			}

			if (value != null)
			{
				Object internalValue;
				try
				{
					internalValue = Convert.ChangeType(value, type);
				}
				catch (ArgumentException)
				{
					throw new ArgumentException($@"默认值 {(value + "" == "" ? "空字符串" : value)} 与类型 {type.ToString()} 不存在显式或隐式转换");
				}
				Value = internalValue;
			}
			else
			{
				if (type.IsValueType)
				{
					if (type == typeof(Boolean))
					{
						Value = false;
					}
					else if (type == typeof(Guid))
					{
						Value = default(Guid);
					}
					else if (type.IsNumeric())
					{
						Value = 0;
					}
				}
				else if (type.IsClass)
				{
					if (type == typeof(String))
					{
						Value = "";
					}
					else
					{
						Value = null;
					}
				}
			}
		}

		/// <summary>
		/// 初始化一个DefaultValueAttribute类的实例
		/// </summary>
		/// <param name="type">默认值类型</param>
		public DefaultValueAttribute(Type type) : this(type, default(Object))
		{

		}

		/// <summary>
		/// 默认值
		/// </summary>
		public Object Value { get; private set; }

		public Type Type { get; private set; }

		internal override Int32 Order => 2;

		internal override String FailReason(String fieldName)
		{
			return $@"{fieldName} 的默认值从{Value}类型转为{_changedProperty.Type.Name}时失败";
		}

		internal override Boolean IsValidate(ChangedProperty property)
		{
			try
			{
				_changedProperty = property;

				//判断DefaultValueAttribute中传入的Type与EntityBase子类属性中的数据的实际类型
				//防止在DateTime类型的属性上使用DefaultValueAttribute向内传入不能转换成DateTime的数据，包括类似于"0000/00/01 00:00:00","WASD","123123"等无法转换为DateTime的数据
				if (property.Type != Type)
				{
					return false;
				}

				Convert.ChangeType(Value, property.Type);
				return true;
			}
			catch (Exception)
			{
				return false;
			}
		}
	}
}
