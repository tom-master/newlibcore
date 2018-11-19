using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
	public class DateTimeDefaultValueAttribute: PropertyDefaultValueAttribute
	{
		public DateTimeDefaultValueAttribute() : base(typeof(DateTime), DateTime.Now)
		{

		}
	}
}
