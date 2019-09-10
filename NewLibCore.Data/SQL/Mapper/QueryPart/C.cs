using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;

namespace NewLibCore.Data.SQL.Mapper.QueryPart
{

    public class C<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal C(SegmentManager segmentManager, ExecutionCore executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
        }

        public D<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return new D<TModel>(_segmentManager, _executionCore);
        }
    }
}
