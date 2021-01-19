using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Store
{
    internal class ComponentBase
    {
        protected internal Expression Expression { get; set; }
    }


    /// <summary>
    /// 表达式拆分后的语句对象
    /// </summary>
    internal class AliasComponentBase : ComponentBase
    {
        protected internal IReadOnlyList<KeyValuePair<String, String>> AliasNameMapper { get; set; }
    }

    /// <summary>
    /// 连接语句对象
    /// </summary>
    internal class JoinComponent : AliasComponentBase
    {
        protected internal String MainTable { get; set; }

        protected internal JoinRelation JoinRelation { get; set; }
    }


    /// <summary>
    /// 排序语句对象
    /// </summary>
    internal class OrderComponent : ComponentBase
    {
        protected internal OrderByType OrderBy { get; set; }
    }

    /// <summary>
    /// 分页语句对象
    /// </summary>
    internal class PaginationComponent : ComponentBase
    {
        internal Int32 Index { get; set; }

        internal Int32 Size { get; set; }

        internal Int32 MaxKey { get; set; }

        internal KeyValuePair<String, String> QueryMainTable { get; set; }
    }

    /// <summary>
    /// 简单语句对象
    /// </summary>
    internal class SimpleComponent : AliasComponentBase
    {

    }

    internal class FromComponent : ComponentBase
    {
        protected internal KeyValuePair<String, String> MainTableMapper { get; set; }
    }

    internal class RawSqlComponent
    {
        internal String Sql { get; set; }
        internal IEnumerable<MapperParameter> Parameters { get; set; }
    }
}