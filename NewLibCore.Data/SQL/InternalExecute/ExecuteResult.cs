using NewLibCore.Data.SQL.InternalTranslation;
using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore.Data.SQL.InternalExecute
{
    internal class TranslationResult
    {
        internal StringBuilder SqlStore { get; private set; }

        internal IList<EntityParameter> ParameterStore { get; private set; }

        internal TranslationResult()
        {
            SqlStore = new StringBuilder();
            ParameterStore = new List<EntityParameter>();
        }

        internal void Append(String sql)
        {
            SqlStore.Append(sql);
        }

        internal void AppendParameter(params EntityParameter[] mapper)
        {
            foreach (var item in mapper)
            {
                ParameterStore.Add(item);
            }
        }

        internal void Clear()
        {
            SqlStore.Clear();
            ParameterStore.Clear();
        }
    }
}
