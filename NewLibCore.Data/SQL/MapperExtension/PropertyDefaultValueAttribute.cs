using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
    public class PropertyDefaultValueAttribute : PropertyValidate
    {
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
                Value = internalValue;
            }
            else
            {
                if (type.BaseType == typeof(ValueType))
                {
                    if (type == typeof(Boolean))
                    {
                        Value = false;
                    }
                    else
                    {
                        Value = 0;
                    }
                }
                else
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
            Type = type;
        }

        public PropertyDefaultValueAttribute(Type type) : this(type, default(Object))
        {

        }

        public Type Type { get; private set; }

        public Object Value { get; private set; }


        public override Int32 Order => 2;
         

        public override String FailReason(String fieldName)
        {
            return $@"{fieldName} 的默认值类型转换失败";
        }

        public override bool IsValidate(object value)
        {
            if (value == null)
            {
                value = Value;
            }

            try
            {
                Convert.ChangeType(value, Type);
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }
    }
}
