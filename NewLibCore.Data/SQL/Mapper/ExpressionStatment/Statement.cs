using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.Mapper.ExpressionStatment
{
    /// <summary>
    /// 表达式拆分后的语句对象
    /// </summary>
    internal abstract class Statement
    {
        protected internal Expression Expression { get; set; }

        protected internal IReadOnlyList<KeyValuePair<String, String>> AliaNameMapper { get; set; }
    }

    /// <summary>
    /// 连接语句对象
    /// </summary>
    internal class JoinExpressionMapper : Statement
    {
        protected internal String MainTable { get; set; }

        protected internal JoinRelation JoinRelation { get; set; }
    }


    /// <summary>
    /// 排序语句对象
    /// </summary>
    internal class OrderExpressionMapper : Statement
    {
        protected internal OrderByType OrderBy { get; set; }
    }

    /// <summary>
    /// 分页语句对象
    /// </summary>
    internal class PaginationExpressionMapper : Statement
    {
        internal Int32 Index { get; set; }

        internal Int32 Size { get; set; }
    }

    /// <summary>
    /// 简单语句对象
    /// </summary>
    internal class SimpleExpressionMapper : Statement
    {

    }
}