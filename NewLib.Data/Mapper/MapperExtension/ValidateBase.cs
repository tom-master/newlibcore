﻿using System;

namespace NewLib.Data.Mapper.MapperExtension
{
	[AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
	public abstract class ValidateBase: Attribute
	{
		public abstract Boolean IsValidate(Object value);

		public abstract Int32 Order { get; }

		public abstract String FailReason(String fieldName);
	}
}
