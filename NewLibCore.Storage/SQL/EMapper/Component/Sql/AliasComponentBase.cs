using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    /// <summary>
    /// 表达式拆分后的语句对象
    /// </summary>
    internal class AliasComponentBase : ComponentBase
    {
        protected internal AliasComponentBase(Expression expression) : base(expression)
        {
            AliasNameMappers = new List<KeyValuePair<String, String>>();
        }
        protected internal IList<KeyValuePair<String, String>> AliasNameMappers { get; private set; }

        internal void InitAliasNameMappers(params KeyValuePair<String, String>[] keyValues)
        {
            foreach (var item in keyValues)
            {
                AliasNameMappers.Add(item);
            }
        }
    }
}