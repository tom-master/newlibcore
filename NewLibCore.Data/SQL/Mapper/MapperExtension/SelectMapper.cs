using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    public sealed class SelectMapper<TModel> /*: ISelectMapper<TModel>*/ where TModel : new()
    {
        internal SelectMapper(ExecutionCore executionCore)
        {
            ExecutionCore = executionCore;
            SegmentManager = MapperConfig.ServiceProvider.GetService<SegmentManager>();
        }

        internal ExecutionCore ExecutionCore { get; private set; }

        internal SegmentManager SegmentManager { get; private set; }

        internal SelectMapper<TModel> From()
        {
            SegmentManager.Add<TModel>();
            return this;
        }

        public SelectMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public SelectMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public SelectMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public SelectMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
          where TLeft : new()
          where TRight : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public SelectMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : new()
            where TRight : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public SelectMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : new()
            where TRight : new()
        {
            Parameter.Validate(expression);
            SegmentManager.Add(expression, JoinType.INNER);

            return this;
        }

    }

    public static class Queryable
    {

        public static SelectMapper<TModel> Select<TModel>(this SelectMapper<TModel> mapper, Expression<Func<TModel, dynamic>> fields = null) where TModel : new()
        {
            if (fields != null)
            {
                mapper.SegmentManager.Add(fields);
            }

            return mapper;
        }

        public static SelectMapper<TModel> OrderByDesc<TModel, TKey>(this SelectMapper<TModel> mapper, Expression<Func<TModel, TKey>> order) where TModel : new()
        {
            Parameter.Validate(order);
            mapper.SegmentManager.AddOrderBy(order, OrderByType.DESC);

            return mapper;
        }

        public static SelectMapper<TModel> OrderByAsc<TModel, TKey>(this SelectMapper<TModel> mapper, Expression<Func<TModel, TKey>> order) where TModel : new()
        {
            Parameter.Validate(order);
            mapper.SegmentManager.AddOrderBy(order, OrderByType.ASC);

            return mapper;
        }

        public static SelectMapper<TModel> Page<TModel>(this SelectMapper<TModel> mapper, Int32 pageIndex, Int32 pageSize) where TModel : new()
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            mapper.SegmentManager.AddPage(pageIndex, pageSize);

            return mapper;
        }


        public static Boolean Exist<TModel>(this SelectMapper<TModel> mapper) where TModel : new()
        {
            return Count(mapper) > 0;
        }

        public static Int32 Count<TModel>(this SelectMapper<TModel> mapper) where TModel : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                mapper.Select(s => "COUNT(*)");
                var executeResult = InternalExecuteSql(mapper);
                return executeResult.ToPrimitive<Int32>();
            });
        }

        public static TModel FirstOrDefault<TModel>(this SelectMapper<TModel> mapper) where TModel : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql(mapper);
                return executeResult.ToSingle<TModel>();
            });
        }

        //public static T FirstOrDefault<T, TModel>(this SelectMapper<TModel> mapper) where T : new() where TModel : new()
        //{
        //    return RunDiagnosis.Watch(() =>
        //    {
        //        var executeResult = InternalExecuteSql<TModel>();
        //        return executeResult.ToSingle<T>();
        //    });
        //}

        public static List<TModel> ToList<TModel>(this SelectMapper<TModel> mapper) where TModel : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql(mapper);
                return executeResult.ToList<TModel>();
            });
        }

        //public List<T> ToList<T>() where T : new()
        //{
        //    return RunDiagnosis.Watch(() =>
        //    {
        //        var executeResult = InternalExecuteSql();
        //        return executeResult.ToList<T>();
        //    });
        //}

        private static RawExecuteResult InternalExecuteSql<TModel>(SelectMapper<TModel> mapper) where TModel : new()
        {
            Handler builder = new SelectHandler<TModel>(mapper.SegmentManager);
            var translationResult = builder.GetTranslationResult();
            return translationResult.ExecuteTranslateResult(mapper.ExecutionCore);
        }
    }
}
