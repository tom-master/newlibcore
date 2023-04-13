using System;
using System.Linq;
using System.Linq.Expressions;
using Google.Protobuf.WellKnownTypes;
using Microsoft.Extensions.Options;

namespace NewLibCore.Storage.SQL.EMapper
{
    internal class EMQueryProvider: IQueryProvider
    {
        private readonly IOptions<EntityMapperOptions> _options;
        internal EMQueryProvider(IOptions<EntityMapperOptions> options)
        {
            _options = options;
        }

        public IQueryable CreateQuery(Expression expression)
        {
            throw new NotImplementedException();
        }

        public IQueryable<TElement> CreateQuery<TElement>(Expression expression)
        {
            return new EMQueryable<TElement>(this, expression);
        }

        public object Execute(Expression expression)
        {
            throw new NotImplementedException();
        }

        public TResult Execute<TResult>(Expression expression)
        {
            return new EMExecutor<TResult>(expression, _options).Execute();
        }
    }
}
