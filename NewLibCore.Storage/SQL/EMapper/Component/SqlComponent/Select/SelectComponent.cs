using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Validate;
using NewLibCore.Validate;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Storage.SQL.Component
{
    public class SelectComponent : PredicateExpressionTranslator, IEntityMapperExecutor
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

        public SelectComponent(IOptions<EntityMapperOptions> options, ResultExecutor resultExecutor)
        : base(options)
        {
            Check.IfNullOrZero(options);
            _options = options.Value;
            JoinComponents = new List<JoinComponent>();
            _resultExecutor = resultExecutor;
        }

        public SelectComponent Query<TModel>() where TModel : EntityBase, new()
        {
            FromComponent = new FromComponent();
            FromComponent.AddFrom<TModel>();
            return this;
        }

        public SelectComponent LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.LEFT);
            JoinComponents.Add(joinComponent);
            return this;
        }

        public SelectComponent RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.RIGHT);
            JoinComponents.Add(joinComponent);
            return this;
        }

        public SelectComponent InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            var joinComponent = new JoinComponent();
            joinComponent.AddJoin(join, JoinRelation.INNER);
            JoinComponents.Add(joinComponent);
            return this;
        }

        public SelectComponent Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);
            PaginationComponent = new PaginationComponent();
            PaginationComponent.AddPagination(pageIndex, pageSize, maxKey);
            return this;
        }

        public SelectComponent Select<TModel>(Expression<Func<TModel, dynamic>> selector = null) where TModel : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddColumnField(selector);
            }

            return this;
        }

        public SelectComponent Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddColumnField(selector);
            }
            return this;
        }
        public SelectComponent Select<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddColumnField(selector);
            }
            return this;
        }

        public SelectComponent Select<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddColumnField(selector);
            }
            return this;
        }

        public SelectComponent Select<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            if (selector != null)
            {
                ColumnFieldComponent = new ColumnFieldComponent();
                ColumnFieldComponent.AddColumnField(selector);
            }
            return this;
        }

        public SelectComponent Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectComponent Where<TModel1, TModel2>(Expression<Func<TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectComponent Where<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectComponent Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectComponent Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent = new WhereComponent();
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectComponent ThenByDesc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent = new OrderComponent();
            OrderComponent.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public SelectComponent ThenByAsc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent = new OrderComponent();
            OrderComponent.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public SelectComponent Include<TModel, TModel1>(Expression<Func<TModel, TModel1>> include) where TModel : EntityBase, new()
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(include);
            var joinComponent = new JoinComponent();
            joinComponent.AddInclude(include);
            JoinComponents.Add(joinComponent);
            return this;
        }

        public ExecutorResult Execute()
        {
            return RunDiagnosis.Watch(() =>
             {
                 if (!FromComponent.AliasNameMappers.Any())
                 {
                     throw new ArgumentException("From");
                 }
                 var mainTable = FromComponent.AliasNameMappers[0];
                 var selectStatement = _options.TemplateBase.CreateSelect(ExtractSelectFields(), mainTable.Key, mainTable.Value);
                 var statementResultBuilder = Translate(selectStatement, WhereComponent, FromComponent, JoinComponents);
                 JoinComponents.Clear();
                 if (PaginationComponent != null)
                 {
                     if (OrderComponent == null)
                     {
                         throw new Exception("Order");
                     }
                     var (fields, tableName) = ExtractOrderFields();
                     var orderTemplate = _options.TemplateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");
                     _options.TemplateBase.CreatePagination(PaginationComponent, orderTemplate, statementResultBuilder.StatmentTemplate);

                 }
                 else if (OrderComponent != null)
                 {
                     var (fields, tableName) = ExtractOrderFields();
                     var orderTemplate = _options.TemplateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");
                     selectStatement.Append(orderTemplate);
                 }

                 return _resultExecutor.Execute(statementResultBuilder);
             });
        }


        private (String Fields, String AliasName) ExtractOrderFields()
        {
            var fields = (LambdaExpression)OrderComponent.Expression;
            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var aliasName = fields.Parameters[0].Type.GetEntityBaseAliasName().AliasName;
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, aliasName);
            }

            throw new Exception("��֧�ֵ� ORDER BY ����ʽ");
        }

        /// <summary>
        /// ��ȡ��Select�ֶ�
        /// </summary>
        /// <returns></returns>
        private String ExtractSelectFields()
        {
            var anonymousObjFields = new List<String>();

            if (ColumnFieldComponent != null)
            {
                var fields = (LambdaExpression)ColumnFieldComponent.Expression;
                if (fields.Body.NodeType == ExpressionType.Constant)
                {
                    var bodyArguments = (fields.Body as ConstantExpression);
                    anonymousObjFields.Add(bodyArguments.Value.ToString());
                }
                else
                {
                    var bodyArguments = (fields.Body as NewExpression).Arguments;
                    foreach (var item in bodyArguments)
                    {
                        var member = (MemberExpression)item;
                        var fieldName = ((ParameterExpression)member.Expression).Type.GetEntityBaseAliasName().AliasName;
                        anonymousObjFields.Add($@"{fieldName}.{member.Member.Name}");
                    }
                }
            }
            else
            {
                var types = GetParameterTypes();
                var tableNames = types.Select(s => new KeyValuePair<String, String>(s.Name, s.GetEntityBaseAliasName().AliasName)).ToList();
                anonymousObjFields = types
                    .SelectMany(s => s.GetProperties(BindingFlags.Instance | BindingFlags.Public)
                    .Where(w => w.GetAttributes<PropertyValidateAttribute>().Any())
                  .Select(s1 => $@"{tableNames.FirstOrDefault(w => w.Key == s.Name).Value}.{s1.Name}")).Distinct().ToList();
            }

            return String.Join(",", anonymousObjFields);
        }


        internal IList<Type> GetParameterTypes()
        {
            var types = new List<Type>();
            if (FromComponent != null)
            {
                var type = (FromComponent.Expression as LambdaExpression).Parameters[0].Type;
                types.Add(type);
            }

            foreach (var item in JoinComponents)
            {
                if (item.Expression == null)
                {
                    continue;
                }
                foreach (var parameter in (item.Expression as LambdaExpression).Parameters)
                {
                    types.Add(parameter.Type);
                }
            }
            return types.Distinct().ToList();
        }


    }
}