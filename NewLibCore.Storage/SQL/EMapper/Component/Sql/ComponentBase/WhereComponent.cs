
using System;
using System.Linq.Expressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql.ComponentBase
{
    internal class WhereComponent : ExpressionComponent
    {
        internal void AddWhere<TModel1>(Expression<Func<TModel1, Boolean>> filter) where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            Expression = filter;
            InitAliasNameMappers(ParseToAliasNames(filter).ToArray());
        }

        internal void AddWhere<TModel1, TModel2>(Expression<Func<TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            Expression = filter;
            InitAliasNameMappers(ParseToAliasNames(filter).ToArray());
        }

        internal void AddWhere<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            Expression = filter;
            InitAliasNameMappers(ParseToAliasNames(filter).ToArray());
        }

        internal void AddWhere<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            Expression = filter;
            InitAliasNameMappers(ParseToAliasNames(filter).ToArray());
        }

        internal void AddWhere<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            Expression = filter;
            InitAliasNameMappers(ParseToAliasNames(filter).ToArray());
        }

        internal void AddWhere<TModel1, TModel2, TModel3, TModel4, TModel5, TModel6>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, TModel6, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        where TModel6 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            Expression = filter;
            InitAliasNameMappers(ParseToAliasNames(filter).ToArray());
        }
    }
}