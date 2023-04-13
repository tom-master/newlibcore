using System;
using System.Linq;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.EMapper
{
    public static class EMQueryableExtension
    {
        public static IQueryable<T> InnerJoin<T, R>(this IQueryable<T> source, Expression<Func<T, R, Boolean>> expression)
        {
            var generic = new Func<IQueryable<T>, Expression<Func<T, R, Boolean>>, IQueryable<T>>(InnerJoin).Method;
            return source.Provider.CreateQuery<T>(
                 Expression.Call(
                     null,
                     generic,
                     source.Expression, Expression.Quote(expression)
                     ));
        }
    }
}
