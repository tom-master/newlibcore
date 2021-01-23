using System;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Component.Sql
{

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
}