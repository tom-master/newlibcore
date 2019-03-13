using NewLibCore.Data.SQL.BuildExtension;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.SQL.DataStore
{
    internal class JoinStatementStore
    {
        internal JoinType JoinType { get; set; }

        internal IList<KeyValuePair<String, String>> AliasNameMappers { get; set; } = new List<KeyValuePair<String, String>>();

        internal Expression Expression { get; set; }

        internal String AliasName { get { return Guid.NewGuid().ToString().Replace("-", ""); } }
    }
}
