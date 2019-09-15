using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public interface IOrder<TModel> where TModel : new()
    {
        IQuery<TModel> ThenDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new();

        IQuery<TModel> ThenAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new();
    }

    public class Order<TModel> : IOrder<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;

        internal Order(SegmentManager segmentManager)
        {
            _segmentManager = segmentManager;
        }

        public IQuery<TModel> ThenDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return new Query<TModel>(_segmentManager);
        }

        public IQuery<TModel> ThenAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return new Query<TModel>(_segmentManager);
        }
    }
}
