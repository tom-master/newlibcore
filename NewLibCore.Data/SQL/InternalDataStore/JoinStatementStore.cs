using NewLibCore.Data.SQL.BuildExtension;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    internal class JoinStatementStore
    {
        internal JoinType JoinType { get; set; }

        public IList<KeyValuePair<String, String>> AliasNameMappers { get; set; } = new List<KeyValuePair<String, String>>();

        public Expression Expression { get; set; }

        public String AliasName { get { return Guid.NewGuid().ToString().Replace("-", ""); } }
    }
}
