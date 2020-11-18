using System;
using System.Collections.Generic;

namespace NewLibCore.Storage.SQL.Component
{
    internal abstract class AliasNameComponent
    {
        protected internal IList<KeyValuePair<String, String>> AliasNameMappers { get; } = new List<KeyValuePair<String, String>>();

        internal virtual void InitAliasNameMappers(params KeyValuePair<String, String>[] keyValues)
        {
            foreach (var item in keyValues)
            {
                AliasNameMappers.Add(item);
            }
        }
    }
}