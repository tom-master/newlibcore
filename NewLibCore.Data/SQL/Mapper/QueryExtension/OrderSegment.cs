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
        

        internal OrderSegment(SegmentManager segmentManager)
        {
            _segmentManager = segmentManager;
        }

        public FinalQuery<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);

            return new FinalQuery<TModel>(_segmentManager);
        }

        public FinalQuery<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return new FinalQuery<TModel>(_segmentManager);
        }

        public FinalQuery<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return new FinalQuery<TModel>(_segmentManager);
        }
    }
}
