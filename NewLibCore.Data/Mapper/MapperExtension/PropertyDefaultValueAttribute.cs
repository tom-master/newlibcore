using System;

namespace NewLibCore.Data.Mapper.MapperExtension
{
    public class PropertyDefaultValueAttribute : ValidateBase
    {
        private readonly Type _type;

        private readonly Object _value;


        public PropertyDefaultValueAttribute(Object value) : this(value.GetType(), value)
        {

        }

        public PropertyDefaultValueAttribute(Type type, Object value)
        {

            if (type == null)
            {
                throw new ArgumentException($@"{nameof(type)} 不能为空");
            }

            if (type.BaseType == typeof(Enum))
            {
                if (value == default(Object))
                {
                    throw new ArgumentException($@"枚举类型 {type.ToString()} 的默认值必须被手动指定");
                }
            }

            if (value != default(Object))
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
                _value = internalValue;
            }
            else
            {
                if (type.BaseType == typeof(ValueType))
                {
                    if (type == typeof(Boolean))
                    {
                        _value = false;
                    }
                    else
                    {
                        _value = 0;
                    }
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
            }
            _type = type;
        }

        public PropertyDefaultValueAttribute(Type type) : this(type, default(Object))
        {

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
