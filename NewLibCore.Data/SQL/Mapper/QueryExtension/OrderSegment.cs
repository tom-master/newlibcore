using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public class OrderSegment<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal OrderSegment(SegmentManager segmentManager, ExecutionCore executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
        }

        public FinalQuery<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);

            return new FinalQuery<TModel>(_segmentManager, _executionCore);
        }

        public FinalQuery<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return new FinalQuery<TModel>(_segmentManager, _executionCore);
        }

        public FinalQuery<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return new FinalQuery<TModel>(_segmentManager, _executionCore);
        }
    }
}
