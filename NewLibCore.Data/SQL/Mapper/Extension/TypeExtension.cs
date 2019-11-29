using System;
using System.Linq;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    internal static class EntityTypeExtension
    {
        /// <summary>
        /// 获取设置在实体的指定表名
        /// </summary>
        /// <param name="t">对象类型</param>
        /// <returns></returns>
        internal static (String TableName, String AliasName) GetTableName(this Type t)
        {
            Parameter.Validate(t);

            var attrubutes = t.GetCustomAttributes(typeof(TableNameAttribute), true);
            if (!attrubutes.Any())
            {
                throw new Exception($@"{t.Name}没有被{nameof(TableNameAttribute)}所修饰");
            }
            var attribute = (TableNameAttribute)attrubutes.FirstOrDefault();
            return (attribute.TableName, attribute.AliasName);
        }
    }
}