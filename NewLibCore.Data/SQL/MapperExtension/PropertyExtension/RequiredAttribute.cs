using System;

namespace NewLibCore.Data.SQL.MapperExtension.PropertyExtension
{
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
            //if (String.IsNullOrEmpty(value + ""))
            //{
            //    return false;
            //}

            //var isComplexType = TypeDescriptor.GetConverter(value.GetType()).CanConvertFrom(typeof(String));
            //if (!isComplexType)
            //{
            //    var objType = value.GetType();
            //    if (objType.IsArray || objType.GetGenericTypeDefinition() == typeof(IList<>))
            //    {
            //        return !(((IList)value).Count == 0);
            //    }
            //}
            //return true;
        }
    }
}
