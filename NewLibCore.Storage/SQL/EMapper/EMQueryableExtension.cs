using System;
using System.Linq;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.EMapper
{
    public static class EMQueryableExtension
    {
        public static IQueryable<T> Inner<T, R>(this IQueryable<T> source, Expression<Func<T, R, bool>> expression)
        {
            var generic = new Func<IQueryable<T>, Expression<Func<T, R, bool>>, IQueryable<T>>(Inner).Method;
            return source.Provider.CreateQuery<T>(
                 Expression.Call(
                     null,
                     generic,
                     source.Expression, Expression.Quote(expression)
                     ));
        }
        public static IQueryable<T> Left<T, R>(this IQueryable<T> source, Expression<Func<T, R, bool>> expression)
        {
            var generic = new Func<IQueryable<T>, Expression<Func<T, R, bool>>, IQueryable<T>>(Left).Method;
            return source.Provider.CreateQuery<T>(
                 Expression.Call(
                     null,
                     generic,
                     source.Expression, Expression.Quote(expression)
                     ));
        }
        public static IQueryable<T> Right<T, R>(this IQueryable<T> source, Expression<Func<T, R, bool>> expression)
        {
            var generic = new Func<IQueryable<T>, Expression<Func<T, R, bool>>, IQueryable<T>>(Right).Method;
            return source.Provider.CreateQuery<T>(
                 Expression.Call(
                     null,
                     generic,
                     source.Expression, Expression.Quote(expression)
                     ));
        }
    }
}
