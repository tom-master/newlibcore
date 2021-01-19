using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Store
{
    internal class ComponentBase
    {
        protected internal Expression Expression { get; private set; }
        protected internal ComponentBase(Expression expression)
        {
            Expression = expression;
        }
    }


    /// <summary>
    /// 表达式拆分后的语句对象
    /// </summary>
    internal class AliasComponentBase : ComponentBase
    {
        protected internal AliasComponentBase(Expression expression) : base(expression)
        {
            AliasNameMappers = new List<KeyValuePair<String, String>>();
        }
        protected internal IList<KeyValuePair<String, String>> AliasNameMappers { get; private set; }

        internal void InitAliasNameMappers(params KeyValuePair<String, String>[] keyValues)
        {
            foreach (var item in keyValues)
            {
                AliasNameMappers.Add(item);
            }
        }
    }

    /// <summary>
    /// 连接语句对象
    /// </summary>
    internal class JoinComponent : AliasComponentBase
    {
        protected internal String MainTable { get; private set; }

        protected internal JoinRelation JoinRelation { get; private set; }

        protected internal JoinComponent(Expression expression, JoinRelation joinRelation, String mainTable) : base(expression)
        {
            MainTable = mainTable;
            JoinRelation = joinRelation;
        }
    }


    /// <summary>
    /// 排序语句对象
    /// </summary>
    internal class OrderComponent : ComponentBase
    {
        protected internal OrderByType OrderBy { get; private set; }

        protected internal OrderComponent(Expression expression, OrderByType orderByType) : base(expression)
        {
            OrderBy = orderByType;
        }

    }

    /// <summary>
    /// 分页语句对象
    /// </summary>
    internal class PaginationComponent : AliasComponentBase
    {
        internal Int32 Index { get; private set; }

        internal Int32 Size { get; private set; }

        internal Int32 MaxKey { get; private set; }

        protected internal PaginationComponent(Int32 index, Int32 size, Int32 maxKey) : base(null)
        {
            Index = index;
            Size = size;
            MaxKey = maxKey;
        }
    }

    /// <summary>
    /// 简单语句对象
    /// </summary>
    internal class SimpleComponent : AliasComponentBase
    {
        protected internal SimpleComponent(Expression expression) : base(expression) { }
    }

    internal class RawSqlComponent
    {
        internal String Sql { get; set; }
        internal IEnumerable<MapperParameter> Parameters { get; set; }
    }
}