using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Storage.SQL.Validate;
using NewLibCore.Validate;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    public class SelectWrapper
    {
        internal SelectComponent SelectComponent { get; private set; }
        internal FromComponent FromComponent { get; private set; }
        internal JoinComponent JoinComponent { get; private set; }
        internal WhereComponent WhereComponent { get; private set; }
        internal OrderComponent OrderComponent { get; private set; }
        internal PaginationComponent PaginationComponent { get; private set; }

        internal TemplateBase _templateBase;
        internal ConditionProcessor _conditionProcessor;

        public SelectWrapper(TemplateBase templateBase, ConditionProcessor conditionProcessor)
        {
            Check.IfNullOrZero(templateBase);
            Check.IfNullOrZero(conditionProcessor);

            _conditionProcessor = conditionProcessor;
            _templateBase = templateBase;

            SelectComponent = new SelectComponent();
            FromComponent = new FromComponent();
            JoinComponent = new JoinComponent();
            WhereComponent = new WhereComponent();
            OrderComponent = new OrderComponent();
        }

        public SelectWrapper Query<TModel>() where TModel : EntityBase, new()
        {
            FromComponent.AddFrom<TModel>();
            return this;
        }

        public SelectWrapper LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public SelectWrapper RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.RIGHT);
            return this;
        }

        public SelectWrapper InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Check.IfNullOrZero(join);
            JoinComponent.AddJoin(join, JoinRelation.INNER);
            return this;
        }

        public SelectWrapper Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);
            var paginationComponent = new PaginationComponent();
            paginationComponent.AddPagination(pageIndex, pageSize, maxKey);
            return this;
        }

        public SelectWrapper Select<TModel>(Expression<Func<TModel, dynamic>> selector = null) where TModel : EntityBase, new()
        {
            if (selector != null)
            {
                SelectComponent.AddSelect(selector);
            }

            return this;
        }

        public SelectWrapper Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            if (selector != null)
            {
                SelectComponent.AddSelect(selector);
            }
            return this;
        }
        public SelectWrapper Select<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> selector = null)
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

        public SelectWrapper Select<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> selector = null)
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

        public SelectWrapper Select<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> selector = null)
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

        public SelectWrapper Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectWrapper Where<TModel1, TModel2>(Expression<Func<TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectWrapper Where<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectWrapper Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            WhereComponent.AddWhere(filter);
            return this;
        }

        public SelectWrapper Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
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

        public SelectWrapper ThenByDesc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public SelectWrapper ThenByAsc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(order);
            OrderComponent.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public SelectWrapper Include<TModel, TModel1>(Expression<Func<TModel, TModel1>> include) where TModel : EntityBase, new()
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(include);
            JoinComponent.AddInclude(include);
            return this;
        }

        /// <summary>
        /// 执行查询操作的翻译
        /// </summary>
        /// <returns></returns>
        public SqlExecuteResultConvert Execute()
        {
            if (!FromComponent.AliasNameMappers.Any())
            {
                throw new ArgumentException("没有指定From表");
            }
            var mainTable = FromComponent.AliasNameMappers[0];
            _templateBase.CreateSelect(ExtractSelectFields(), mainTable.Key, mainTable.Value);
            var result = _conditionProcessor.Process(JoinComponent, WhereComponent, FromComponent);

            if (PaginationComponent != null)
            {
                if (OrderComponent == null)
                {
                    throw new Exception("分页中没有指定排序字段");
                }
                var (fields, tableName) = ExtractOrderFields();
                var orderTemplate = _templateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");

                var newSql = _templateBase.CreatePagination(PaginationComponent, orderTemplate, result.ToString());
                result.ClearSql();
                result.Append(newSql);
            }
            else if (OrderComponent != null)
            {
                var (fields, tableName) = ExtractOrderFields();
                var orderTemplate = _templateBase.CreateOrderBy(OrderComponent.OrderBy, $@"{tableName}.{fields}");
                result.Append(orderTemplate);
            }

            return result.Execute();
        }

        /// <summary>
        /// 提取出排序字段
        /// </summary>
        /// <returns></returns>
        private (String Fields, String AliasName) ExtractOrderFields()
        {
            var fields = (LambdaExpression)OrderComponent.Expression;
            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var aliasName = fields.Parameters[0].Type.GetEntityBaseAliasName().AliasName;
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, aliasName);
            }

            throw new Exception("不支持的 ORDER BY 表达式");
        }

        /// <summary>
        /// 提取出Select字段
        /// </summary>
        /// <returns></returns>
        private String ExtractSelectFields()
        {
            var anonymousObjFields = new List<String>();

            if (SelectComponent != null)
            {
                var fields = (LambdaExpression)SelectComponent.Expression;
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

            if (JoinComponent.JoinComponents.Any())
            {
                foreach (var item in JoinComponent.JoinComponents)
                {
                    foreach (var parameter in (item.Expression as LambdaExpression).Parameters)
                    {
                        types.Add(parameter.Type);
                    }
                }
            }
            return types.Distinct().ToList();
        }
    }
}