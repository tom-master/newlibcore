using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.SQL.MapperExtension
{
    public class RequiredAttribute : PropertyValidate
    {
        public override Int32 Order => 3;


        public override String FailReason(String fieldName)
        {
            return $@"{fieldName} 为必填项!";
        }

        public override Boolean IsValidate(Object value)
        {
            var isComplexType = TypeDescriptor.GetConverter(value.GetType()).CanConvertFrom(typeof(String));
            if (!isComplexType)
            {
                var objType = value.GetType();
                if (objType.IsArray || (((TypeInfo)objType).ImplementedInterfaces as IList<Type>).Any(a => a == typeof(IList) || a == typeof(ICollection) || a == typeof(IEnumerable)))
                {
                    return !(((IList)value).Count == 0);
                }
            }

            return !String.IsNullOrEmpty(value + "");
        }
    }
}
