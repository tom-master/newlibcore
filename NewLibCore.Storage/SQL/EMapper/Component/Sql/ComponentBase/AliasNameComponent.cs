using System;
using System.Collections.Generic;

namespace NewLibCore.Storage.SQL.Component.Sql.ComponentBase
{
    internal abstract class AliasNameComponent
    {
        protected internal IList<KeyValuePair<String, String>> AliasNameMappers { get; private set; }

        internal virtual void InitAliasNameMappers(params KeyValuePair<String, String>[] keyValues)
        {
            foreach (var item in keyValues)
            {
                AliasNameMappers.Add(item);
            }
        }
    }
}