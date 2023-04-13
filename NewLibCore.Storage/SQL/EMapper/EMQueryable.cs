using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;

namespace NewLibCore.Storage.SQL.EMapper
{
    internal class EMQueryable<T>: IQueryable<T>
    {
        internal EMQueryable(IOptions<EntityMapperOptions> options)
        {
            Provider = new EMQueryProvider(options);
            Expression = Expression.Constant(this);
        }

        internal EMQueryable(IQueryProvider queryProvider, Expression expression)
        {
            Provider = queryProvider;
            Expression = expression;
        }

        public Type ElementType => typeof(T);

        public Expression Expression
        {
            get; private set;
        }

        public IQueryProvider Provider
        {
            get; private set;
        }

        public IEnumerator<T> GetEnumerator()
        {
            return (Provider.Execute<IEnumerable<T>>(Expression)).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return (Provider.Execute<IEnumerable>(Expression)).GetEnumerator();
        }
    }
}
