using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.Mapper.ExpressionStatment
{
    /// <summary>
    /// 表达式拆分后的语句对象
    /// </summary>
    internal abstract class ExpressionMapper
    {
        protected internal Expression Expression { get; set; }

        protected internal IReadOnlyList<KeyValuePair<String, String>> AliaNameMapper { get; set; }
    }
}