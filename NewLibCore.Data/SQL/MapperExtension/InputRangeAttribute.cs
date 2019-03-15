using System;

namespace NewLibCore.Data.SQL.MapperExtension
{

    public class InputRangeAttribute : PropertyValidate
    {
        private readonly Int32 _min;

        private readonly Int32 _max;


        public override Int32 Order => 1;


        public InputRangeAttribute(Int32 min, Int32 max)
        {
            _min = min;
            _max = max;
        }

        public InputRangeAttribute(Int32 max, Boolean canbeEmpty = false) : this(0, max)
        {
        }

        public override Boolean IsValidate(Object value)
        {
            var internalValue = (value + "").ToString();
            /*if (_canbeEmpty && String.IsNullOrEmpty(internalValue))
            {
                return true;
            }*/

            var valueLength = internalValue.Length;

            if (_min == 0 && valueLength <= _max)
            {
                return true;
            }

            if (valueLength < _min || valueLength > _max)
            {
                return false;
            }

            if (valueLength >= _min && valueLength <= _max)
            {
                return true;
            }

            return false;
        }

        public override String FailReason(String fieldName)
        {
            return $@"{fieldName} 的长度不符合特性的预设长度区间，预设长度区间为为 最小长度：{_min} 最大长度：{_max}";
        }
    }
}
