using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NewLibCore.Data.SQL.Mapper;

namespace NewLibCore.Data.SQL.Translation
{
    internal class TranslationCoreResult
    {
        internal StringBuilder SqlStore { get; private set; }

        internal IList<EntityParameter> ParameterStore { get; private set; }

        internal TranslationCoreResult()
        {
            SqlStore = new StringBuilder();
            ParameterStore = new List<EntityParameter>();
        }

        internal void Append(String sql, IEnumerable<EntityParameter> entityParameters = null)
        {
            SqlStore.Append(sql);
            if (entityParameters != null)
            {
                foreach (var item in entityParameters)
                {
                    ParameterStore.Add(item);
                }
            }
        }

        internal void Append(params EntityParameter[] entityParameters)
        {
            Append(entityParameters.ToList());
        }

        internal void Append(IEnumerable<EntityParameter> entityParameters)
        {
            if (entityParameters != null)
            {
                foreach (var item in entityParameters)
                {
                    ParameterStore.Add(item);
                }
            }
        }

        internal void Clear()
        {
            SqlStore.Clear();
            ParameterStore.Clear();
        }
    }
}
