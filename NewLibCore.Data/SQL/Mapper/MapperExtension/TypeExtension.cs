using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    public static class TypeExtension
    {
        public static String GetAliasName(this Type t)
        {
            var attrubutes = t.GetCustomAttributes(typeof(TableNameAttribute), true);
            if (!attrubutes.Any())
            {
                return t.Name;
            }

            return ((TableNameAttribute)attrubutes.FirstOrDefault()).TableName;
        }
    }
}