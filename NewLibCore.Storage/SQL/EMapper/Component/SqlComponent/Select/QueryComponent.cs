using System;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;
namespace NewLibCore.Storage.SQL.Component
{
    public class QueryComponent: ExpressionTranslator, IEntityMapperExecutor
    {
        internal ColumnFieldComponent ColumnFieldComponent { get; private set; }
        internal PaginationComponent PaginationComponent { get; private set; }
        internal RootComponent RootComponent { get; private set; } = new RootComponent();
        internal OrderComponent OrderComponent { get; private set; } = new OrderComponent();

        public string ComponentIdentity => this.GetType().Name;

        internal readonly EntityMapperOptions _options;
        private readonly ResultExecutor _resultExecutor;

        public QueryComponent(IOptions<EntityMapperOptions> options, ResultExecutor resultExecutor)
        : base(options)
        {
            Check.IfNullOrZero(options);
            _options = options.Value;
            _resultExecutor = resultExecutor;
        }

        public QueryComponent Model<TModel>() where TModel : EntityBase, new()
        {
            Expression<Func<TModel, TModel>> expression = (a) => a;
            RootComponent.AddExpression(expression, EMType.FROM);
            return this;
        }

        public QueryComponent LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, bool>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            RootComponent.AddExpression(join, EMType.LEFT);
            return this;
        }

        public QueryComponent RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, bool>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            RootComponent.AddExpression(join, EMType.RIGHT);
            return this;
        }

        public QueryComponent InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, bool>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            RootComponent.AddExpression(join, EMType.INNER);
            return this;
        }

        public QueryComponent Page(int pageIndex, int pageSize, int maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);
            PaginationComponent = new PaginationComponent();
            PaginationComponent.AddPagination(pageIndex, pageSize, maxKey);
            return this;
        }

        public QueryComponent Select<TModel>(Expression<Func<TModel, dynamic>> selector = null) where TModel : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddExpression(selector, EMType.SELECT);
            }

            return this;
        }

        public QueryComponent Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddExpression(selector, EMType.SELECT);
            }
            return this;
        }
        public QueryComponent Select<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddExpression(selector, EMType.SELECT);
            }
            return this;
        }

        public QueryComponent Select<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddExpression(selector, EMType.SELECT);
            }
            return this;
        }

        public QueryComponent Select<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddExpression(selector, EMType.SELECT);
            }
            return this;
        }

        public QueryComponent Where<TModel1>(Expression<Func<TModel1, bool>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            RootComponent.AddExpression(filter, EMType.WHERE);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2>(Expression<Func<TModel1, TModel2, bool>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            RootComponent.AddExpression(filter, EMType.WHERE);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, bool>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            RootComponent.AddExpression(filter, EMType.WHERE);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, bool>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            RootComponent.AddExpression(filter, EMType.WHERE);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, bool>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            RootComponent.AddExpression(filter, EMType.WHERE);
            return this;
        }

        public QueryComponent ThenByDesc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent.AddExpression(order, EMType.DESC);
            return this;
        }

        public QueryComponent ThenByAsc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent.AddExpression(order, EMType.ASC);
            return this;
        }

        public ExecutorResult Execute()
        {
            throw new Exception();
            //return RunDiagnosis.Watch(() =>
            // {
            //     var mainTable = RootComponent.GetMainTable();
            //     var selectStatement = _options.TemplateBase.CreateSelect(ColumnFieldComponent?.ExtractSelectFields(), mainTable.Key, mainTable.Value);
            //     var statementResultBuilder = Translate(null);
            //     statementResultBuilder.AddStatementTemplate(selectStatement);

            //     if (PaginationComponent != null)
            //     {
            //         if (OrderComponent == null)
            //         {
            //             throw new Exception("Order");
            //         }
            //         var (fields, tableName) = OrderComponent.ExtractOrderFields(OrderComponent.OrderBy);
            //         var orderTemplate = _options.TemplateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");
            //         _options.TemplateBase.CreatePagination(PaginationComponent, orderTemplate, statementResultBuilder.StatmentTemplate);

            //     }
            //     else if (OrderComponent != null)
            //     {
            //         var (fields, tableName) = OrderComponent.ExtractOrderFields(OrderComponent.OrderBy);
            //         var orderTemplate = _options.TemplateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");
            //         selectStatement.Append(orderTemplate);
            //     }
               
            //     return _resultExecutor.Execute(statementResultBuilder);
            // });
        }
    }
}