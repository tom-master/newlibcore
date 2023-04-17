using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Storage.SQL.Validate;
using NewLibCore.Validate;
namespace NewLibCore.Storage.SQL.Extension
{
    internal static class EntityTypeExtension
    {
        private static readonly IDictionary<string, KeyValuePair<string, string>> _dic = new Dictionary<string, KeyValuePair<string, string>>();

        /// <summary>
        /// 获取设置在EntityBase的表别名
        /// </summary>
        /// <param name="entityBase"></param>
        /// <returns></returns>
        internal static (string TableName, string AliasName) GetEntityBaseAliasName(this EntityBase entityBase)
        {
            return GetEntityBaseAliasName(entityBase.GetType());
        }

        /// <summary>
        /// 获取设置在EntityBase的表别名
        /// </summary>
        /// <param name="t">对象类型</param>
        /// <returns></returns>
        internal static (string TableName, string AliasName) GetEntityBaseAliasName(this Type t)
        {
            Check.IfNullOrZero(t);
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