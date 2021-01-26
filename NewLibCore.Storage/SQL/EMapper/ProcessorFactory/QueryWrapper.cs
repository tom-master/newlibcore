using System;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    public class QueryWrapper<TModel> where TModel : EntityBase, new()
    {
        internal SelectComponent SelectComponent { get; private set; }
        internal FromComponent FromComponent { get; private set; }
        internal JoinComponent JoinComponent { get; private set; }
        internal WhereComponent WhereComponent { get; private set; }
        internal OrderComponent OrderComponent { get; private set; }

        internal QueryWrapper()
        {
            SelectComponent = new SelectComponent();
            FromComponent = new FromComponent();
            JoinComponent = new JoinComponent();
            WhereComponent = new WhereComponent();
            OrderComponent = new OrderComponent();
        }

        public QueryWrapper<TModel> Query()
        {
            FromComponent.AddFrom<TModel>();
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.RIGHT);
            return this;
        }

        public QueryWrapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.RIGHT);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.INNER);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.INNER);
            return this;
        }

        public QueryWrapper<TModel> Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);
            var paginationComponent = new PaginationComponent();
            paginationComponent.AddPagination(pageIndex, pageSize, maxKey);
            return this;
        }

        public QueryWrapper<TModel> Select(Expression<Func<TModel, dynamic>> selector = null)
        {
            if (selector != null)
            {
                SelectComponent.AddSelect(selector);
            }

            return this;
        }

        public QueryWrapper<TModel> Select<TModel1>(Expression<Func<TModel, TModel1, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        {
            if (selector != null)
            {
                SelectComponent.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            if (selector != null)
            {
                SelectComponent.AddSelect(selector);
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
                SelectComponent.AddSelect(selector);
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
                SelectComponent.AddSelect(selector);
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
                SelectComponent.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Where(Expression<Func<TModel, Boolean>> filter)
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel, TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2>(Expression<Func<TModel, TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3>(Expression<Func<TModel, TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
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
            WhereComponent.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Check.IfNullOrZero(order);
            OrderComponent.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public QueryWrapper<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Check.IfNullOrZero(order);
            OrderComponent.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public QueryWrapper<TModel> Include<TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(include);
            JoinComponent.AddInclude(include);
            return this;
        }
    }
}