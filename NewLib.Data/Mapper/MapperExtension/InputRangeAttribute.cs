using System;

namespace NewLib.Data.Mapper.MapperExtension
{

	public class InputRangeAttribute: ValidateBase
	{
		private Int32 _min;

		private Int32 _max;

		public override Int32 Order
		{
			get { return 1; }
		}

		public InputRangeAttribute(Int32 min, Int32 max)
		{
			_min = min;
			_max = max;
		}

		public InputRangeAttribute(Int32 max)
		{
			_min = 0;
			_max = max;
		}

		public override Boolean IsValidate(Object value)
		{
			if (value == null)
			{
				return false;
			}

			var internalValue = value.ToString();

			var valueLength = internalValue.Length;

			if (_min == 0 && valueLength <= _max)
			{
				return true;
			}

			if (valueLength < _min || valueLength > _max)
			{
				return false;
			}

			if (valueLength > _min && valueLength < _max)
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
