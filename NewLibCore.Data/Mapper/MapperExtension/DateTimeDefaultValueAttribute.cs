using System;

namespace NewLibCore.Data.Mapper.MapperExtension
{
	public class DateTimeDefaultValueAttribute: PropertyDefaultValueAttribute
	{
		public DateTimeDefaultValueAttribute() : base(typeof(DateTime), DateTime.Now)
		{

		}
	}
}
