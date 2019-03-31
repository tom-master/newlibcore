using System;

namespace NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)]
    public abstract class PropertyValidate : Attribute
    {
        public virtual Boolean IsValidate(Object value) { return false; }

        public virtual Int32 Order { get { return -1; } }

        public virtual String FailReason(String fieldName) { return fieldName; }
    }
}
