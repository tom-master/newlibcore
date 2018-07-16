﻿using System;

namespace NewLib.Data.Mapper.MapperExtension
{

	public class PropertyRequiredAttribute: ValidateBase
	{
		public override Int32 Order
		{
			get { return 3; }
		}

		public override String FailReason(String fieldName)
		{
			return $@"{fieldName} 为必填项!";
		}

		public override bool IsValidate(object value)
		{
			return !(value == null);
		}
	}
}
