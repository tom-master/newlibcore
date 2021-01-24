using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.Component.Sql.ComponentBase;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    public class QueryWrapper<TModel> where TModel : EntityBase, new()
    {
        private readonly Processor _processor;

        private readonly SelectComponent _selectComponent;

        internal QueryWrapper(Processor processor)
        {
            Check.IfNullOrZero(processor);

            _processor = processor;
            _selectComponent = new SelectComponent();
        }

        public QueryWrapper<TModel> Query()
        {
            var fromComponent = new FromComponent();
            fromComponent.AddFrom<TModel>();
            _selectComponent.AddFromComponent(fromComponent);
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.LEFT);
            _selectComponent.AddJoinComponent(joinComponent);
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.LEFT);
            _selectComponent.AddJoinComponent(joinComponent);
            return this;
        }

        public QueryWrapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.RIGHT);
            _selectComponent.AddJoinComponent(joinComponent);
            return this;
        }

        public QueryWrapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.RIGHT);
            _selectComponent.AddJoinComponent(joinComponent);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.INNER);
            _selectComponent.AddJoinComponent(joinComponent);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.INNER);
            _selectComponent.AddJoinComponent(joinComponent);
            return this;
        }

        public QueryWrapper<TModel> Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);
            var paginationComponent = new PaginationComponent();
            paginationComponent.AddPagination(pageIndex, pageSize, maxKey);
            _selectComponent.AddPaginationComponent(paginationComponent);
            return this;
        }

        public QueryWrapper<TModel> Select(Expression<Func<TModel, dynamic>> selector = null)
        {
            if (selector != null)
            {
                _selectComponent.AddSelect(selector);
            }

            return this;
        }

        public QueryWrapper<TModel> Select<TModel1>(Expression<Func<TModel, TModel1, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        {
            if (selector != null)
            {
                _selectComponent.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            if (selector != null)
            {
                _selectComponent.AddSelect(selector);
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
                _selectComponent.AddSelect(selector);
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
                _selectComponent.AddSelect(selector);
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
                _selectComponent.AddSelect(selector);
            }
            return this;
        }

        public QueryWrapper<TModel> Where(Expression<Func<TModel, Boolean>> filter)
        {
            Check.IfNullOrZero(filter);
            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(filter);
            _selectComponent.AddWhereComponent(whereComponent);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(filter);
            _selectComponent.AddWhereComponent(whereComponent);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel, TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(filter);
            _selectComponent.AddWhereComponent(whereComponent);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2>(Expression<Func<TModel, TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(filter);
            _selectComponent.AddWhereComponent(whereComponent);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3>(Expression<Func<TModel, TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(filter);
            _selectComponent.AddWhereComponent(whereComponent);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(filter);
            _selectComponent.AddWhereComponent(whereComponent);
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
            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(filter);
            _selectComponent.AddWhereComponent(whereComponent);
            return this;
        }

        public QueryWrapper<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Check.IfNullOrZero(order);
            var orderComponent = new OrderComponent();
            orderComponent.AddOrderBy(order, OrderByType.DESC);
            _selectComponent.AddOrderComponent(orderComponent);
            return this;
        }

        public QueryWrapper<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Check.IfNullOrZero(order);
            var orderComponent = new OrderComponent();
            orderComponent.AddOrderBy(order, OrderByType.ASC);
            _selectComponent.AddOrderComponent(orderComponent);
            return this;
        }

        public QueryWrapper<TModel> Include<TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(include);

            var joinComponent = new JoinComponent();
            joinComponent.AddInclude(include);
            _selectComponent.AddJoinComponent(joinComponent);
            return this;
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process().FirstOrDefault<TModel>();
            });
        }

        public TResult FirstOrDefault<TResult>() where TResult : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process().FirstOrDefault<TResult>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process().ToList<TModel>();
            });
        }

        public List<TResult> ToList<TResult>() where TResult : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process().ToList<TResult>();
            });
        }

        public Int32 Count()
        {
            return RunDiagnosis.Watch(() =>
            {
                Select((a) => "COUNT(1)");
                return _processor.Process().FirstOrDefault<Int32>();
            });
        }
    }
}