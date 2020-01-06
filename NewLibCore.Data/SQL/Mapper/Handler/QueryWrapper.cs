using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Component.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{

    public class QueryWrapper<TModel> where TModel : EntityBase, new()
    {
        private readonly RunDiagnosis _diagnosis;

        private readonly ExpressionStore _expressionStore;

        private readonly QueryCacheBase _queryCacheBase;

        private readonly QueryHandler _queryHandler;

        internal QueryWrapper(ExpressionStore expressionStore, RunDiagnosis runDiagnosis, QueryCacheBase queryCacheBase, QueryHandler queryHandler)
        {
            Parameter.Validate(expressionStore);
            Parameter.Validate(runDiagnosis);
            Parameter.Validate(queryCacheBase);
            Parameter.Validate(queryHandler);

            _expressionStore = expressionStore;

            _diagnosis = runDiagnosis;
            _queryCacheBase = queryCacheBase;
            _queryHandler = queryHandler;
        }

        public QueryWrapper<TModel> Query()
        {
            _expressionStore.AddFrom<TModel>();
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Parameter.Validate(join);
            _expressionStore.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Parameter.Validate(join);
            _expressionStore.AddJoin(join, JoinRelation.LEFT);

            return this;
        }

        public QueryWrapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Parameter.Validate(join);
            _expressionStore.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Parameter.Validate(join);
            _expressionStore.AddJoin(join, JoinRelation.RIGHT);

            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Parameter.Validate(join);
            _expressionStore.AddJoin(join, JoinRelation.INNER);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Parameter.Validate(join);
            _expressionStore.AddJoin(join, JoinRelation.INNER);

            return this;
        }

        public QueryWrapper<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);

            var maxKey = _queryCacheBase.Get<Int32>("mysql-max-primarykey");
            _expressionStore.AddPage(pageIndex, pageSize, (Int32)maxKey);

            return this;
        }

        public QueryWrapper<TModel> Select(Expression<Func<TModel, dynamic>> selector = null)
        {
            if (selector != null)
            {
                _expressionStore.AddSelect(selector);
            }

            return this;
        }

        public QueryWrapper<TModel> Select<TModel1>(Expression<Func<TModel, TModel1, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        {
            if (selector != null)
            {
                _expressionStore.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            if (selector != null)
            {
                _expressionStore.AddSelect(selector);
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
                _expressionStore.AddSelect(selector);
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
                _expressionStore.AddSelect(selector);
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
                _expressionStore.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Where(Expression<Func<TModel, Boolean>> filter)
        {
            Parameter.Validate(filter);
            _expressionStore.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Parameter.Validate(filter);
            _expressionStore.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel, TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Parameter.Validate(filter);
            _expressionStore.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2>(Expression<Func<TModel, TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Parameter.Validate(filter);
            _expressionStore.AddWhere(filter);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3>(Expression<Func<TModel, TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Parameter.Validate(filter);
            _expressionStore.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Parameter.Validate(filter);
            _expressionStore.AddWhere(filter);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Parameter.Validate(filter);
            _expressionStore.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public QueryWrapper<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public QueryWrapper<TModel> Include<TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel1 : EntityBase, new()
        {
            Parameter.Validate(include);
            _expressionStore.AddInclude(include);
            return this;
        }

        public TModel FirstOrDefault()
        {
            return _diagnosis.Watch(() =>
            {
                return _queryHandler.Execute(_expressionStore).FirstOrDefault<TModel>();
            });
        }

        public TResult FirstOrDefault<TResult>() where TResult : new()
        {
            return _diagnosis.Watch(() =>
            {
                return _queryHandler.Execute(_expressionStore).FirstOrDefault<TResult>();
            });
        }

        public List<TModel> ToList()
        {
            return _diagnosis.Watch(() =>
            {
                return _queryHandler.Execute(_expressionStore).ToList<TModel>();
            });
        }

        public List<TResult> ToList<TResult>() where TResult : new()
        {
            return _diagnosis.Watch(() =>
            {
                return _queryHandler.Execute(_expressionStore).ToList<TResult>();
            });
        }

        public Int32 Count()
        {
            return _diagnosis.Watch(() =>
            {
                Select((a) => "COUNT(*)");
                return _queryHandler.Execute(_expressionStore).FirstOrDefault<Int32>();
            });
        }
    }
}
