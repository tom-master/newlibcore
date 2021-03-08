using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Validate;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class JoinComponent : ComponentBase
    {
        internal String MainTable { get; private set; }

        internal JoinRelation JoinRelation { get; private set; }

        internal IList<JoinComponent> JoinComponents { get; private set; }

        internal void AddJoin<TModel, TJoin>(Expression<Func<TModel, TJoin, Boolean>> expression, JoinRelation joinRelation)
                where TModel : EntityBase, new()
                where TJoin : EntityBase, new()
        {
            Check.IfNullOrZero(expression);

            Expression = expression;
            JoinRelation = joinRelation;
            MainTable = typeof(TModel).GetEntityBaseAliasName().TableName;

            InitAliasNameMappers(ParseToAliasNames(expression).ToArray().ToArray());
            JoinComponents.Add(this);
        }

        internal void AddInclude<TModel, TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel : EntityBase, new()
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(include);

            var parameterType = include.Parameters[0].Type;
            var foreignKeyType = include.Body.Type;

            //找到模型中用ForeignKeyAttribute修饰的外键
            var foreignKeyPropertyInfo = parameterType.GetProperties(BindingFlags.Instance | BindingFlags.Public)
                .FirstOrDefault(w => w.GetAttributes<ForeignKeyAttribute>().Any(f => f.ForeignType == foreignKeyType));
            if (foreignKeyPropertyInfo == null)
            {
                throw new ArgumentException($@"{parameterType.Name}中没有用{nameof(ForeignKeyAttribute)}修饰的属性");
            }

            //找到对应外键的表用PrimaryKeyAttribute修饰的主键
            var foreignPropertyInfo = foreignKeyType.GetProperties(BindingFlags.Instance | BindingFlags.Public)
            .FirstOrDefault(w => w.GetAttributes<PrimaryKeyAttribute>().Any());
            if (foreignPropertyInfo == null)
            {
                throw new ArgumentException($@"{foreignKeyType.Name}中没有用{nameof(PrimaryKeyAttribute)}修饰的属性");
            }

            var leftParameter = Expression.Parameter(parameterType, parameterType.GetEntityBaseAliasName().AliasName);
            var rightParameter = Expression.Parameter(foreignKeyType, foreignKeyType.GetEntityBaseAliasName().AliasName);

            var left = Expression.Property(leftParameter, foreignKeyPropertyInfo);
            var right = Expression.Property(rightParameter, foreignPropertyInfo);

            var includeExpression = Expression.Lambda<Func<TModel, TModel1, Boolean>>(Expression.Equal(left, right), leftParameter, rightParameter);
            AddJoin(includeExpression, JoinRelation.LEFT);
        }
    }
}