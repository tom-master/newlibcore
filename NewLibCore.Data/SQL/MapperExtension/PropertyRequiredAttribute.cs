using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
    public class PropertyRequiredAttribute : PropertyValidate
    {
        public override Int32 Order => 3;
       

        public override String FailReason(String fieldName)
        {
            return $@"{fieldName} 为必填项!";
        }

        public override bool IsValidate(object value)
        {
            return !String.IsNullOrEmpty(value + "");
        }
    }
}
