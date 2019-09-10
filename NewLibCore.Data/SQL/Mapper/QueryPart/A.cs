using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.QueryPart
{
    public class A<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal A(SegmentManager segmentManager, ExecutionCore executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
        }

        public C<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new C<TModel>(_segmentManager, _executionCore);
        }

        public C<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new C<TModel>(_segmentManager, _executionCore);
        }

        public C<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new C<TModel>(_segmentManager, _executionCore);
        }
    }
}
