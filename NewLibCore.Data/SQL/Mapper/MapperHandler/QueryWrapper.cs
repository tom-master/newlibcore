using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{

    public class QueryWrapper<TModel> where TModel : new()
    {
        private readonly ExpressionStore _expressionStore;

        private readonly IServiceProvider _serviceProvider;

        internal QueryWrapper(ExpressionStore expressionStore, IServiceProvider serviceProvider)
        {
            _expressionStore = expressionStore;
            _serviceProvider = serviceProvider;
        }

        public QueryWrapper<TModel> Query()
        {
            _expressionStore.AddFrom<TModel>();
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
        where TLeft : new()
        where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddJoin(expression, JoinRelation.LEFT);

            return this;
        }

        public QueryWrapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
        where TLeft : new()
        where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddJoin(expression, JoinRelation.RIGHT);

            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression)
        where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddJoin(expression, JoinRelation.INNER);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
        where TLeft : new()
        where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddJoin(expression, JoinRelation.INNER);

            return this;
        }

        public QueryWrapper<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);

            _expressionStore.AddPage(pageIndex, pageSize);
            return this;
        }

        public QueryWrapper<TModel> Select(Expression<Func<TModel, dynamic>> fields)
        {
            if (fields != null)
            {
                _expressionStore.AddSelect(fields);
            }

            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        {
            if (fields != null)
            {
                _expressionStore.AddSelect(fields);
            }
            return this;
        }
        public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        {
            if (fields != null)
            {
                _expressionStore.AddSelect(fields);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        {
            if (fields != null)
            {
                _expressionStore.AddSelect(fields);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        where TModel5 : new()
        {
            if (fields != null)
            {
                _expressionStore.AddSelect(fields);
            }
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel1, Boolean>> expression)
        where TModel1 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddWhere(expression);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2>(Expression<Func<TModel1, TModel2, Boolean>> expression)
        where TModel1 : new()
        where TModel2 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddWhere(expression);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, Boolean>> expression)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddWhere(expression);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, Boolean>> expression)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddWhere(expression);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> expression)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        where TModel5 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.AddWhere(expression);
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

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                Handler handler = new QueryHandler<TModel>(_expressionStore, _serviceProvider);
                return handler.Execute().FirstOrDefault<TModel>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                Handler handler = new QueryHandler<TModel>(_expressionStore, _serviceProvider);
                return handler.Execute().ToList<TModel>();
            });
        }
    }

}
