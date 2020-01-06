﻿using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
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

        private readonly IServiceProvider _serviceProvider;

        private readonly QueryCacheBase _queryCacheBase;

        internal QueryWrapper(ExpressionStore expressionStore, IServiceProvider serviceProvider)
        {
            Parameter.Validate(expressionStore);
            Parameter.Validate(serviceProvider);

            _expressionStore = expressionStore;
            _serviceProvider = serviceProvider;
            _diagnosis = _serviceProvider.GetService<RunDiagnosis>();
            _queryCacheBase = _serviceProvider.GetService<QueryCacheBase>();
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

            var maxKey = _queryCacheBase.Get("mysql-max-primarykey");
            if ((Int32)maxKey == 0)
            {
                maxKey = 0;
            }
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
                HandlerBase handler = new QueryHandler(_expressionStore, _serviceProvider);
                return handler.Process().FirstOrDefault<TModel>();
            });
        }

        public TResult FirstOrDefault<TResult>() where TResult : new()
        {
            return _diagnosis.Watch(() =>
            {
                HandlerBase handler = new QueryHandler(_expressionStore, _serviceProvider);
                return handler.Process().FirstOrDefault<TResult>();
            });
        }

        public List<TModel> ToList()
        {
            return _diagnosis.Watch(() =>
            {
                HandlerBase handler = new QueryHandler(_expressionStore, _serviceProvider);
                return handler.Process().ToList<TModel>();
            });
        }

        public List<TResult> ToList<TResult>() where TResult : new()
        {
            return _diagnosis.Watch(() =>
            {
                HandlerBase handler = new QueryHandler(_expressionStore, _serviceProvider);
                return handler.Process().ToList<TResult>();
            });
        }

        public Int32 Count()
        {
            return _diagnosis.Watch(() =>
            {
                Select((a) => "COUNT(*)");
                HandlerBase handler = new QueryHandler(_expressionStore, _serviceProvider);
                return handler.Process().FirstOrDefault<Int32>();
            });
        }
    }
}
