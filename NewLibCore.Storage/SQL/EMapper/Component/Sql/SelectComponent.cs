using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Component.Sql.ComponentBase;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{

    internal class SelectComponent : ExpressionComponent
    {
        internal OrderComponent OrderComponent { get; private set; }

        internal FromComponent FromComponent { get; private set; }

        internal WhereComponent WhereComponent { get; private set; }

        internal PaginationComponent PaginationComponent { get; private set; }

        internal JoinComponent JoinComponent { get; private set; }


        internal void AddOrderComponent(OrderComponent orderComponent)
        {
            Check.IfNullOrZero(orderComponent);
            OrderComponent = orderComponent;
        }

        internal void AddFromComponent(FromComponent fromComponent)
        {
            Check.IfNullOrZero(fromComponent);
            FromComponent = fromComponent;
        }

        internal void AddWhereComponent(WhereComponent whereComponent)
        {
            Check.IfNullOrZero(whereComponent);
            WhereComponent = whereComponent;
        }

        internal void AddPaginationComponent(PaginationComponent paginationComponent)
        {
            Check.IfNullOrZero(paginationComponent);
            PaginationComponent = paginationComponent;
        }

        internal void AddJoinComponent(JoinComponent joinComponent)
        {
            Check.IfNullOrZero(joinComponent);
            JoinComponent = joinComponent;
        }

        internal void AddSelect<TModel1>(Expression<Func<TModel1, dynamic>> selector)
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }

        internal void AddSelect<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }

        internal void AddSelect<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }

        internal void AddSelect<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }

        internal void AddSelect<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }

        internal IList<KeyValuePair<String, String>> MergeComponentAlias()
        {
            var newAliasMapper = new List<KeyValuePair<String, String>>();
            if (WhereComponent != null)
            {
                newAliasMapper.AddRange(WhereComponent.AliasNameMappers);
            }
            if (JoinComponent.JoinComponents.Any())
            {
                newAliasMapper.AddRange(JoinComponent.JoinComponents.SelectMany(s => s.AliasNameMappers));
            }
            if (FromComponent != null)
            {
                newAliasMapper.AddRange(FromComponent.AliasNameMappers);
            }
            newAliasMapper = newAliasMapper.Select(s => s).Distinct().ToList();

            var sameGroup = newAliasMapper.GroupBy(a => a.Value);
            foreach (var groupItem in sameGroup)
            {
                if (groupItem.Count() > 1)
                {
                    throw new ArgumentException($@"表:{String.Join(",", groupItem.Select(s => s.Key))}指定了相同别名:{groupItem.Key}");
                }
            }

            return newAliasMapper;
        }


        /// <summary>
        /// 合并返回表达式的参数类型
        /// </summary>
        /// <returns></returns>
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