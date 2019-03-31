using System;

namespace NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension
{
    public class DateTimeDefaultValueAttribute : DefaultValueAttribute
    {
        public DateTimeDefaultValueAttribute() : base(typeof(DateTime), DateTime.Now)
        {

        }
    }
}
