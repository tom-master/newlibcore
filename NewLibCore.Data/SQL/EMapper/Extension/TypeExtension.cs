using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using NewLibCore.Data.SQL.Validate;
using NewLibCore.Validate;
namespace NewLibCore.Data.SQL.Extension
{
    internal static class EntityTypeExtension
    {

        private static readonly IDictionary<String, KeyValuePair<String, String>> _dic = new Dictionary<String, KeyValuePair<String, String>>();

        /// <summary>
        /// 获取设置在实体的指定表名
        /// </summary>
        /// <param name="entityBase"></param>
        /// <returns></returns>
        internal static (String TableName, String AliasName) GetTableName(this EntityBase entityBase)
        {
            return GetTableName(entityBase.GetType());
        }

        /// <summary>
        /// 获取设置在实体的指定表名
        /// </summary>
        /// <param name="t">对象类型</param>
        /// <returns></returns>
        internal static (String TableName, String AliasName) GetTableName(this Type t)
        {
            Parameter.IfNullOrZero(t);
            lock (_dic)
            {
                if (t.BaseType != typeof(EntityBase))
                {
                    throw new InvalidOperationException($@"{t.Name}不属于基类:{nameof(EntityBase)}，不是数据实体的一部分，因此不能获取到表名和别名");
                }

                if (_dic.ContainsKey(t.Name))
                {
                    var dic = _dic[t.Name];
                    return (dic.Key, dic.Value);
                }

                var attrubutes = t.GetAttributes<TableNameAttribute>(true);
                if (!attrubutes.Any())
                {
                    throw new Exception($@"{t.Name}没有被{nameof(TableNameAttribute)}所修饰");
                }

                var attribute = attrubutes.FirstOrDefault();

                _dic.Add(t.Name, new KeyValuePair<string, string>(attribute.TableName, attribute.AliasName));
                return (attribute.TableName, attribute.AliasName);
            }
        }
    }
}