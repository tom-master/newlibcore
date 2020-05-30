using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.Store
{
    /// <summary>
    /// 表达式拆分后的语句对象
    /// </summary>
    internal abstract class ExpressionMapperBase
    {
        protected internal Expression Expression { get; set; }

        protected internal IReadOnlyList<KeyValuePair<String, String>> AliaNameMapper { get; set; }
    }

    /// <summary>
    /// 连接语句对象
    /// </summary>
    internal class JoinExpressionMapper : ExpressionMapperBase
    {
        protected internal String MainTable { get; set; }

        protected internal JoinRelation JoinRelation { get; set; }
    }


    /// <summary>
    /// 排序语句对象
    /// </summary>
    internal class OrderExpressionMapper : ExpressionMapperBase
    {
        protected internal OrderByType OrderBy { get; set; }
    }

    /// <summary>
    /// 分页语句对象
    /// </summary>
    internal class PaginationExpressionMapper : ExpressionMapperBase
    {
        internal Int32 Index { get; set; }

        internal Int32 Size { get; set; }

        internal Int32 MaxKey { get; set; }

        internal KeyValuePair<String, String> QueryMainTable { get; set; }
    }

    /// <summary>
    /// 简单语句对象
    /// </summary>
    internal class SimpleExpressionMapper : ExpressionMapperBase
    {

    }

    internal class DirectExpressionMapper
    {
        internal String Sql { get; set; }
        internal IEnumerable<MapperParameter> Parameters { get; set; }
    }

    internal class M
    {
        internal EntityBase Model { get; private set; }

        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Model = model;
        }
    }
}