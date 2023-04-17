using System;

namespace NewLibCore.Storage.SQL.Validate
{
    /// <summary>
    /// 标记被修饰的属性有输入范围
    /// </summary>
    public class InputRangeAttribute : PropertyValidateAttribute
    {
        private readonly int _min;

        private readonly int _max;

        /// <summary>
        /// 初始化一个InputRangeAttribute类的实例
        /// </summary>
        /// <param name="min">最短长度</param>
        /// <param name="max">最大长度</param>
        public InputRangeAttribute(int min, int max)
        {
            _min = min;
            _max = max;
        }

        /// <summary>
        /// 初始化一个InputRangeAttribute类的实例
        /// </summary>
        /// <param name="max">最大长度</param> 
        public InputRangeAttribute(int max) : this(0, max)
        {
        }

        internal override int Order
        {
            get { return 1; }
        }

        internal override bool IsValidate(ChangedProperty property)
        {
            var internalValue = (property.Value + "").ToString();
            /*if (_canbeEmpty && string.IsNullOrEmpty(internalValue))
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

        internal override string FailReason(string fieldName)
        {
            return $@"{fieldName} 的长度不符合特性的预设长度区间，预设长度区间为为 最小长度：{_min} 最大长度：{_max}";
        }
    }
}
