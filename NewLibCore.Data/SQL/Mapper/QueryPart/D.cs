using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.QueryPart
{

    public class D<TModel> : E<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;

        internal D(SegmentManager segmentManager) : base(segmentManager)
        {
            _segmentManager = segmentManager;
        }

        public E<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);

            return new E<TModel>(_segmentManager);
        }

        public E<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return new E<TModel>(_segmentManager);
        }

        public E<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return new E<TModel>(_segmentManager);
        }
    }
}
