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
    public sealed class SelectMapper<TModel> /*: ISelectMapper<TModel>*/ where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal SelectMapper(ExecutionCore executionCore)
        {
            _executionCore = executionCore;
            _segmentManager = MapperConfig.ServiceProvider.GetService<SegmentManager>();
        }

        public B<TModel> From()
        {
            _segmentManager.Add<TModel>();
            return new B<TModel>(_segmentManager, _executionCore);
        }
    }

    public class A<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal A(SegmentManager segmentManager, ExecutionCore executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
        }

        public C<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new C<TModel>(_segmentManager, _executionCore);
        }

        public C<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new C<TModel>(_segmentManager, _executionCore);
        }

        public C<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new C<TModel>(_segmentManager, _executionCore);
        }
    }

    public class B<TModel> : A<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal B(SegmentManager segmentManager, ExecutionCore executionCore) : base(segmentManager, executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
        }

        public B<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public B<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public B<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public B<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public B<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public B<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }
    }

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

    public class D<TModel> : E<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal D(SegmentManager segmentManager, ExecutionCore executionCore) : base(segmentManager, executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
        }

        public E<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);

            return new E<TModel>(_segmentManager, _executionCore);
        }

        public E<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return new E<TModel>(_segmentManager, _executionCore);
        }

        public E<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return new E<TModel>(_segmentManager, _executionCore);
        }
    }

    public class E<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal E(SegmentManager segmentManager, ExecutionCore executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
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

        private RawExecuteResult InternalExecuteSql()
        {
            Handler builder = new SelectHandler<TModel>(_segmentManager);
            var translationResult = builder.GetTranslationResult();
            return translationResult.ExecuteTranslateResult(_executionCore);
        }
    }
}
