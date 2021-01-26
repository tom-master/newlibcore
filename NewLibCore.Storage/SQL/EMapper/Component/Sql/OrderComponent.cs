using System;
using System.Linq.Expressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class OrderComponent : ExpressionComponent
    {
        internal OrderByType OrderBy { get; private set; }
        internal void AddOrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> expression, OrderByType orderByType)
                where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(expression);
            Expression = expression;
            OrderBy = orderByType;
        }
    }
}