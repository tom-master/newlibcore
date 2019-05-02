using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
    internal abstract class Statement
    {
        protected internal Expression Expression { get; set; }

        protected internal KeyValuePair<String, String> AliaNameMapper { get; set; }
    }

    internal class JoinStatement : Statement
    {
        protected internal JoinType JoinType { get; set; }
    }

    internal class OrderStatement : Statement
    {
        protected internal OrderByType OrderBy { get; set; }
    }

    internal class SimpleStatement : Statement
    {

    }

    internal class StatementStore
    {
        internal IList<Statement> Statements { get; private set; }

        internal StatementStore()
        {
            Statements = new List<Statement>();
        }

        internal void AddOrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> order, OrderByType orderByType)
        {
            Parameter.Validate(order);
            Statements.Add(new OrderStatement
            {
                Expression = order,
                OrderBy = orderByType
            });
        }

        internal void Add<TModel, TJoin>(Expression<Func<TModel, TJoin, Boolean>> expression, JoinType joinType = JoinType.NONE) where TModel : PropertyMonitor, new() where TJoin : PropertyMonitor, new()
        {
            Parameter.Validate(expression);

            foreach (var item in expression.Parameters)
            {
                var joinStore = new JoinStatement
                {
                    Expression = expression,
                    JoinType = joinType,
                    AliaNameMapper = new KeyValuePair<string, string>(item.Name, item.Type.Name)
                };
                Statements.Add(joinStore);
            }
        }

        internal void Add<TModel>(Expression<Func<TModel, Boolean>> expression) where TModel : PropertyMonitor, new()
        {
            Parameter.Validate(expression);
            Statements.Add(new SimpleStatement
            {
                Expression = expression
            });
        }

        internal void AddSelectFields<TModl>(Expression<Func<TModl, dynamic>> expression) where TModl : PropertyMonitor, new()
        {
            Parameter.Validate(expression);
            Statements.Add(new SimpleStatement
            {
                Expression = expression
            });
        }

        internal void Clear()
        {
            SelectFields = default;
            ConditionExpression = default;
            OrderByType = default;
            AliasName = default;
            JoinStores.Clear();
        }
    }
}
