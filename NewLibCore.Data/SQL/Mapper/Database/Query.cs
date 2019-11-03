using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public interface IQuery<TModel> where TModel : new()
    {
        TModel FirstOrDefault();

        T FirstOrDefault<T>() where T : new();

        List<TModel> ToList();

        List<T> ToList<T>() where T : new();

        Int32 Count();
    }

    
    public class Query<TModel> : IQuery<TModel> where TModel : new()
    {
        internal readonly ExpressionStore _expressionStore;

        internal Query(ExpressionStore expressionStore)
        {
            _expressionStore = expressionStore;
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
                this.Select((a) => "COUNT(*)");
                var executeResult = InternalExecuteSql();
                return executeResult.FirstOrDefault<Int32>();
            });
        }
    }

    public static class SelectExtension
    {
        public static IQuery<TModel> Page<TModel>(this Query<TModel> query, Int32 pageIndex, Int32 pageSize) where TModel : new()
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            query._expressionStore.AddPage(pageIndex, pageSize);
            return query;
        }

        public static IQuery<TModel> Select<TModel>(this Query<TModel> query, Expression<Func<TModel, dynamic>> fields = null) where TModel : new()
        {
            if (fields != null)
            {
                query._expressionStore.Add(fields);
            }

            return query;
        }

        public static IQuery<TModel> Select<TModel, T, T1>(this Query<TModel> query, Expression<Func<T, T1, dynamic>> fields = null)
        where TModel : new()
        where T : new()
        where T1 : new()
        {
            if (fields != null)
            {
                query._expressionStore.Add(fields);
            }
            return query;
        }

        public static IQuery<TModel> Where<TModel>(this Query<TModel> query, Expression<Func<TModel, Boolean>> expression) where TModel : new()
        {
            Parameter.Validate(expression);
            query._expressionStore.Add(expression);
            return query;
        }

        public static IQuery<TModel> Where<TModel, T>(this Query<TModel> query, Expression<Func<T, Boolean>> expression)
        where TModel : new()
        where T : new()
        {
            Parameter.Validate(expression);
            query._expressionStore.Add(expression);
            return query;
        }

        public static IQuery<TModel> Where<TModel, T>(this Query<TModel> query, Expression<Func<TModel, T, Boolean>> expression)
        where TModel : new()
         where T : new()
        {
            Parameter.Validate(expression);
            query._expressionStore.Add(expression);
            return query;
        }

        public static IQuery<TModel> ThenByDesc<TModel, TKey>(this Query<TModel> query, Expression<Func<TModel, TKey>> order) where TModel : new()
        {
            Parameter.Validate(order);
            query._expressionStore.AddOrderBy(order, OrderByType.DESC);
            return query;
        }

        public static IQuery<TModel> ThenByAsc<TModel, TOrder, TKey>(this Query<TModel> query, Expression<Func<TOrder, TKey>> order)
        where TModel : new()
        where TOrder : new()
        {
            Parameter.Validate(order);
            query._expressionStore.AddOrderBy(order, OrderByType.ASC);
            return query;
        }

        public static IQuery<TModel> ThenByDesc<TModel, TOrder, TKey>(this Query<TModel> query, Expression<Func<TOrder, TKey>> order)
        where TModel : new()
        where TOrder : new()
        {
            Parameter.Validate(order);
            query._expressionStore.AddOrderBy(order, OrderByType.DESC);
            return query;
        }

        public static IQuery<TModel> ThenByAsc<TModel, TKey>(this Query<TModel> query, Expression<Func<TModel, TKey>> order) where TModel : new()
        {
            Parameter.Validate(order);
            query._expressionStore.AddOrderBy(order, OrderByType.ASC);
            return query;
        }
    }

}
