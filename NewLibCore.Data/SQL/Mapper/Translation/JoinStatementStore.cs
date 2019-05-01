﻿using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
    internal class JoinStatementStore
    {
        internal JoinType JoinType { get; set; }

        internal IList<KeyValuePair<String, String>> AliasNameMappers { get; set; } = new List<KeyValuePair<String, String>>();

        internal Expression Expression { get; set; }
        
    }
}
 