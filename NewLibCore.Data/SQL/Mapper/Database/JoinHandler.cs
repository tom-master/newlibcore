using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public class JoinHandler<TModel> where TModel : new()
    {
        private readonly ExpressionStore _expressionStore;
        internal JoinHandler(ExpressionStore expressionStore)
        {
            _expressionStore = expressionStore;
        }

        public JoinHandler<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.LEFT);

            return this;
        }

        public JoinHandler<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.LEFT);

            return this;
        }

        public JoinHandler<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.RIGHT);

            return this;
        }

        public JoinHandler<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.RIGHT);

            return this;
        }

        public JoinHandler<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.INNER);

            return this;
        }

        public JoinHandler<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.INNER);

            return this;
        }

        public JoinHandler<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _expressionStore.AddPage(pageIndex, pageSize);
            return this;
        }

        public JoinHandler<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }

            return this;
        }

        public JoinHandler<TModel> Select<T, T1>(Expression<Func<T, T1, dynamic>> fields = null)
        where T : new()
        where T1 : new()
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }
            return this;
        }

        public JoinHandler<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return this;
        }

        public JoinHandler<TModel> Where<T>(Expression<Func<T, Boolean>> expression)
        where T : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return this;
        }

        public JoinHandler<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression)
         where T : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return this;
        }

        public JoinHandler<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public JoinHandler<TModel> ThenByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order)
        where TOrder : new()
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public JoinHandler<TModel> ThenByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order)
        where TOrder : new()
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public JoinHandler<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.ASC);
            return this;
        }


    }
}
