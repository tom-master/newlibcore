using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Store;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    public class QueryWrapper<TModel> where TModel : EntityBase, new()
    {
        private readonly Processor _processor;

        private readonly ExpressionStore _store;

        internal QueryWrapper(ExpressionStore store, Processor processor)
        {
            Check.IfNullOrZero(store);
            Check.IfNullOrZero(processor);

            _store = store;
            _processor = processor;
        }

        public QueryWrapper<TModel> Query()
        {
            _store.AddFrom<TModel>();
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.LEFT);

            return this;
        }

        public QueryWrapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.RIGHT);

            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.INNER);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.INNER);

            return this;
        }

        public QueryWrapper<TModel> Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);

            _store.AddPage(pageIndex, pageSize, maxKey);
            return this;
        }

        public QueryWrapper<TModel> Select(Expression<Func<TModel, dynamic>> selector = null)
        {
            if (selector != null)
            {
                _store.AddSelect(selector);
            }

            return this;
        }

        public QueryWrapper<TModel> Select<TModel1>(Expression<Func<TModel, TModel1, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        {
            if (selector != null)
            {
                _store.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            if (selector != null)
            {
                _store.AddSelect(selector);
            }
            return this;
        }
        public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            if (selector != null)
            {
                _store.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            if (selector != null)
            {
                _store.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            if (selector != null)
            {
                _store.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Where(Expression<Func<TModel, Boolean>> filter)
        {
            Check.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel, TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2>(Expression<Func<TModel, TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3>(Expression<Func<TModel, TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Check.IfNullOrZero(order);
            _store.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public QueryWrapper<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Check.IfNullOrZero(order);
            _store.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public QueryWrapper<TModel> Include<TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(include);
            _store.AddInclude(include);
            return this;
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).FirstOrDefault<TModel>();
            });
        }

        public TResult FirstOrDefault<TResult>() where TResult : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).FirstOrDefault<TResult>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).ToList<TModel>();
            });
        }

        public List<TResult> ToList<TResult>() where TResult : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).ToList<TResult>();
            });
        }

        public Int32 Count()
        {
            return RunDiagnosis.Watch(() =>
            {
                Select((a) => "COUNT(1)");
                return _processor.Process(_store).FirstOrDefault<Int32>();
            });
        }
    }
}