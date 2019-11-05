using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public class QueryHandlerExtension<TModel> where TModel : new()
    {
        private readonly ExpressionStore _expressionStore;

        internal QueryHandlerExtension(ExpressionStore expressionStore) 
        {
            _expressionStore = expressionStore;
        }

        public QueryHandlerExtension<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.LEFT);

            return this;
        }

        public QueryHandlerExtension<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.LEFT);

            return this;
        }

        public QueryHandlerExtension<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.RIGHT);

            return this;
        }

        public QueryHandlerExtension<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.RIGHT);

            return this;
        }

        public QueryHandlerExtension<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.INNER);

            return this;
        }

        public QueryHandlerExtension<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.INNER);

            return this;
        }

        public QueryHandlerExtension<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _expressionStore.AddPage(pageIndex, pageSize);
            return this;
        }

        public QueryHandlerExtension<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }

            return this;
        }

        public QueryHandlerExtension<TModel> Select<T, T1>(Expression<Func<T, T1, dynamic>> fields = null)
        where T : new()
        where T1 : new()
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }
            return this;
        }

        public QueryHandlerExtension<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return this;
        }

        public QueryHandlerExtension<TModel> Where<T>(Expression<Func<T, Boolean>> expression)
        where T : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return this;
        }

        public QueryHandlerExtension<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression)
         where T : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return this;
        }

        public QueryHandlerExtension<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public QueryHandlerExtension<TModel> ThenByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order)
        where TOrder : new()
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public QueryHandlerExtension<TModel> ThenByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order)
        where TOrder : new()
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public QueryHandlerExtension<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.FirstOrDefault<TModel>();
            });
        }

        public T FirstOrDefault<T>() where T : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.FirstOrDefault<T>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToList<TModel>();
            });
        }

        public List<T> ToList<T>() where T : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToList<T>();
            });
        }

        private RawResult InternalExecuteSql()
        {
            Handler handler = new QueryHandler<TModel>(_expressionStore);
            return handler.Execute();
        }

        public Int32 Count()
        {
            return RunDiagnosis.Watch(() =>
            {
                Select((a) => "COUNT(*)");
                var executeResult = InternalExecuteSql();
                return executeResult.FirstOrDefault<Int32>();
            });
        }

    }
}
