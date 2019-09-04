using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public sealed class SelectMapper<TModel> : ISelectMapper<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;

        private readonly ExecutionCore _executionCore;
        internal SelectMapper(ExecutionCore executionCore)
        {
            _executionCore = executionCore;
            _segmentManager = MapperConfig.ServiceProvider.GetService<SegmentManager>();
        }

        public Boolean Exist()
        {
            return Count() > 0;
        }

        public Int32 Count()
        {
            return RunDiagnosis.Watch(() =>
            {
                Select(s => "COUNT(*)");
                var executeResult = InternalExecuteSql();
                return executeResult.ToPrimitive<Int32>();
            });
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToSingle<TModel>();
            });
        }

        public T FirstOrDefault<T>() where T : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToSingle<T>();
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

        internal SelectMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return this;
        }

        public SelectMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);
            return this;
        }

        public SelectMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public SelectMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public SelectMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public SelectMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
          where TLeft : new()
          where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public SelectMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : new()
            where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public SelectMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : new()
            where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public SelectMapper<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return this;
        }

        public SelectMapper<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return this;
        }

        private RawExecuteResult InternalExecuteSql()
        {
            Handler builder = new SelectHandler<TModel>(_segmentManager);
            var translationResult = builder.GetTranslationResult();
            return translationResult.ExecuteTranslateResult(_executionCore);
        }
    }
}
