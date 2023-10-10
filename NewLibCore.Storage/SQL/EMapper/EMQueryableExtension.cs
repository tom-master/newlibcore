﻿using System;
using System.Linq;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.EMapper
{
    public static class EMQueryableExtension
    {
        public static IQueryable<T> InnerJoin<T, R>(this IQueryable<T> source, Expression<Func<T, R, bool>> expression)
        {
            var generic = new Func<IQueryable<T>, Expression<Func<T, R, bool>>, IQueryable<T>>(InnerJoin).Method;
            return source.Provider.CreateQuery<T>(
                 Expression.Call(
                     null,
                     generic,
                     source.Expression, Expression.Quote(expression)
                     ));
        }
        public static IQueryable<T> LeftJoin<T, R>(this IQueryable<T> source, Expression<Func<T, R, bool>> expression)
        {
            var generic = new Func<IQueryable<T>, Expression<Func<T, R, bool>>, IQueryable<T>>(LeftJoin).Method;
            return source.Provider.CreateQuery<T>(
                 Expression.Call(
                     null,
                     generic,
                     source.Expression, Expression.Quote(expression)
                     ));
        }
        public static IQueryable<T> RightJoin<T, R>(this IQueryable<T> source, Expression<Func<T, R, bool>> expression)
        {
            var generic = new Func<IQueryable<T>, Expression<Func<T, R, bool>>, IQueryable<T>>(RightJoin).Method;
            return source.Provider.CreateQuery<T>(
                 Expression.Call(
                     null,
                     generic,
                     source.Expression, Expression.Quote(expression)
                     ));
        }
    }
}
