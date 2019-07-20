using System;

namespace NewLibCore.Data.SQL.Mapper.AttributeExtension
{
	/// <summary>
	/// 非空验证
	/// </summary>
	public class RequiredAttribute : PropertyValidate
	{
		public override Int32 Order
		{
			get { return 3; }
		}

		public override String FailReason(String fieldName)
		{
			return $@"{fieldName} 为必填项!";
		}

		public override Boolean IsValidate(Object value)
		{
			return !String.IsNullOrEmpty(value + "");
		}
	}
}
