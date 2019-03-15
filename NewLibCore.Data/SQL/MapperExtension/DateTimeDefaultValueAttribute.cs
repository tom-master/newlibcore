using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
	public class DateTimeDefaultValueAttribute: DefaultValueAttribute
	{
		public DateTimeDefaultValueAttribute() : base(typeof(DateTime), DateTime.Now)
		{

		}
	}
}
