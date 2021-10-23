using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Component
{
    public class QueryComponent : PredicateExpressionTranslator, IEntityMapperExecutor
    {
        internal ColumnFieldComponent ColumnFieldComponent { get; private set; }
        internal FromComponent FromComponent { get; private set; }
        internal IList<JoinComponent> JoinComponents { get; private set; }
        internal WhereComponent WhereComponent { get; private set; }
        internal OrderComponent OrderComponent { get; private set; }
        internal PaginationComponent PaginationComponent { get; private set; }

        public string ComponentIdentity => this.GetType().Name;

        internal readonly EntityMapperOptions _options;
        private readonly ResultExecutor _resultExecutor;

        public QueryComponent(IOptions<EntityMapperOptions> options, ResultExecutor resultExecutor)
        : base(options)
        {
            Check.IfNullOrZero(options);
            _options = options.Value;
            JoinComponents = new List<JoinComponent>();
            _resultExecutor = resultExecutor;
        }

        public QueryComponent Query<TModel>() where TModel : EntityBase, new()
        {
            FromComponent = new FromComponent();

            var modelType = typeof(TModel);
            Expression<Func<TModel, TModel>> expression = (a) => a;

            // var (tableName, aliasName) = modelType.GetEntityBaseAliasName();
            // var KeyValuePair = new KeyValuePair<String, String>(tableName, aliasName);
            // Expression = expression;
            // InitAliasNameMappers(KeyValuePair);

            FromComponent.AddExpression(expression);

            // FromComponent.AddFrom<TModel>();
            return this;
        }

        public QueryComponent LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoinType(JoinRelation.LEFT);
            joinComponent.InitMainTableName<TLeft>();
            joinComponent.AddExpression(join);
            JoinComponents.Add(joinComponent);
            return this;
        }

        public QueryComponent RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoinType(JoinRelation.RIGHT);
            joinComponent.InitMainTableName<TLeft>();
            joinComponent.AddExpression(join);
            JoinComponents.Add(joinComponent);
            return this;
        }

        public QueryComponent InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoinType(JoinRelation.INNER);
            joinComponent.InitMainTableName<TLeft>();
            joinComponent.AddExpression(join);
            JoinComponents.Add(joinComponent);
            return this;
        }

        public QueryComponent Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
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
                ColumnFieldComponent.AddExpression(selector);
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
                ColumnFieldComponent.AddExpression(selector);
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
                ColumnFieldComponent.AddExpression(selector);
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
                ColumnFieldComponent.AddExpression(selector);
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
                ColumnFieldComponent.AddExpression(selector);
            }
            return this;
        }

        public QueryComponent Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddExpression(filter);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2>(Expression<Func<TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();

            WhereComponent.AddExpression(filter);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddExpression(filter);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddExpression(filter);
            return this;
        }

        public QueryComponent Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddExpression(filter);
            return this;
        }

        public QueryComponent ThenByDesc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent = new OrderComponent();
            OrderComponent.AddExpression(order);
            OrderComponent.AddOrderType(OrderByType.DESC);
            return this;
        }

        public QueryComponent ThenByAsc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent = new OrderComponent();
            OrderComponent.AddExpression(order);
            OrderComponent.AddOrderType(OrderByType.ASC);
            return this;
        }

        // public QueryComponent Include<TModel, TModel1>(Expression<Func<TModel, TModel1>> include) where TModel : EntityBase, new()
        // where TModel1 : EntityBase, new()
        // {
        //     Check.IfNullOrZero(include);
        //     var joinComponent = new JoinComponent();
        //     joinComponent.AddInclude(include);
        //     JoinComponents.Add(joinComponent);
        //     return this;
        // }

        public ExecutorResult Execute()
        {
            return RunDiagnosis.Watch(() =>
             {
                 if (!FromComponent.AliasNameMappers.Any())
                 {
                     throw new ArgumentException("From");
                 }

                 var mainTable = FromComponent.AliasNameMappers[0];
                 var selectStatement = _options.TemplateBase.CreateSelect(ColumnFieldComponent?.ExtractSelectFields(), mainTable.Key, mainTable.Value);
                 var statementResultBuilder = Translate(selectStatement, this);
                 JoinComponents.Clear();

                 if (PaginationComponent != null)
                 {
                     if (OrderComponent == null)
                     {
                         throw new Exception("Order");
                     }
                     var (fields, tableName) = OrderComponent.ExtractOrderFields();
                     var orderTemplate = _options.TemplateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");
                     _options.TemplateBase.CreatePagination(PaginationComponent, orderTemplate, statementResultBuilder.StatmentTemplate);

                 }
                 else if (OrderComponent != null)
                 {
                     var (fields, tableName) = OrderComponent.ExtractOrderFields();
                     var orderTemplate = _options.TemplateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");
                     selectStatement.Append(orderTemplate);
                 }

                 return _resultExecutor.Execute(statementResultBuilder);
             });
        }

        internal IList<KeyValuePair<String, String>> MergeComponentAlias()
        {
            var newAliasMapper = new List<KeyValuePair<String, String>>();

            if (JoinComponents != null)
            {
                newAliasMapper.AddRange(JoinComponents.SelectMany(s => s.AliasNameMappers));
            }

            if (WhereComponent != null)
            {
                newAliasMapper.AddRange(WhereComponent.AliasNameMappers);
            }

            if (FromComponent != null)
            {
                newAliasMapper.AddRange(FromComponent.AliasNameMappers);
            }
            newAliasMapper = newAliasMapper.Select(s => s).Distinct().ToList();
            CheckDuplicateTableAliasName(newAliasMapper);
            return newAliasMapper;
        }
        private void CheckDuplicateTableAliasName(IEnumerable<KeyValuePair<String, String>> newAliasMapper)
        {
            var sameGroup = newAliasMapper.GroupBy(a => a.Value);
            if (sameGroup.Any(w => w.Count() > 1))
            {
                throw new InvalidOperationException("DuplicateTableAliasName");
            }
        }
    }
}