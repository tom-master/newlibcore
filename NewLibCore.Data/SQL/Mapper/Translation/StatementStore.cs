using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
    internal class StatementStore
    {
        internal Expression SelectFields { get; private set; }

        internal Expression ConditionExpression { get; private set; }

        internal Expression OrderExpression { get; private set; }

        internal KeyValuePair<String, String>? AliasName { get; private set; }

        internal JoinType JoinType { get; private set; }

        internal OrderByType? OrderByType { get; private set; }

        internal IList<StatementStore> JoinStores { get; private set; }

        internal StatementStore()
        {
            JoinStores = new List<StatementStore>();
            AliasName = new KeyValuePair<String, String>();
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

        internal void AddWhere<TModel>(Expression<Func<TModel, Boolean>> expression) where TModel : PropertyMonitor, new()
        {
            ConditionExpression = expression;
        }

        internal void AddSelectFields<TModl>(Expression<Func<TModl, dynamic>> expression) where TModl : PropertyMonitor, new()
        {
            SelectFields = expression;
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

            foreach (var item in expression.Parameters)
            {
                if (typeof(TLeft) == item.Type)
                {
                    continue;
                }
                var joinStore = new StatementStore
                {
                    ConditionExpression = expression,
                    JoinType = joinType,
                    AliasName = new KeyValuePair<string, string>(item.Name, item.Type.Name)
                };
                JoinStores.Add(joinStore);
            }
        }

        internal void Clear()
        {
            SelectFields = default;
            ConditionExpression = default;
            OrderByType = default;
            AliasName = default;
            OrderByType = default;
            JoinStores.Clear();
        }
    }

}
