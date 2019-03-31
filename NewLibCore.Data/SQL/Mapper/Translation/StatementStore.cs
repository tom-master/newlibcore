using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
    internal class StatementStore
    {
        internal Expression Expression { get; private set; }

        internal Expression OrderExpression { get; private set; }

        internal String AliasName { get; set; }

        internal JoinType JoinType { get { return JoinType.NONE; } }

        internal OrderByType? OrderByType { get; private set; }

        internal IList<JoinStatementStore> JoinStores { get; private set; }

        internal StatementStore()
        {
            JoinStores = new List<JoinStatementStore>();
        }

        internal void AddOrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> order, OrderByType orderByType)
        {
            if (order == null)
            {
                throw new ArgumentNullException($@"{order} 不能为null");
            }

            OrderByType = orderByType;
            OrderExpression = order;
        }

        internal void AddWhere<TModel>(Expression<Func<TModel, Boolean>> expression)
        {
            Expression = expression;
        }

        internal void AddJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression, JoinType joinType) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            if (expression == null)
            {
                throw new ArgumentNullException($@"{expression} 不能为null");
            }

            if (joinType == JoinType.NONE)
            {
                throw new ArgumentNullException($@"在决定调用AddJoin时,参数:{nameof(joinType)} 不能为none");
            }

            var joinStore = new JoinStatementStore
            {
                Expression = expression,
                JoinType = joinType
            };
            foreach (var item in expression.Parameters)
            {
                if (typeof(TLeft) == item.Type)
                {
                    continue;
                }
                joinStore.AliasNameMappers.Add(new KeyValuePair<String, String>(item.Name, item.Type.Name));
            }

            JoinStores.Add(joinStore);
        }

        internal void Clear()
        {
            Expression = null;
            OrderByType = null;
            AliasName = "";
            OrderByType = null;
            JoinStores.Clear();
        }
    }
}
