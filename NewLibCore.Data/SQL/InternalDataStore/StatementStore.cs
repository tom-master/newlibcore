using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    internal class StatementStore
    {
        public Expression Expression { get; private set; }

        public String AliasName { get; set; }

        public JoinType JoinType { get { return JoinType.NONE; } }

        public IList<JoinStatementStore> JoinStores { get; private set; }

        public StatementStore()
        {
            JoinStores = new List<JoinStatementStore>();
        }

        public void AddWhere<TModel>(Expression<Func<TModel, Boolean>> expression)
        {
            Expression = expression;
        }

        public void AddJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression, JoinType joinType) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
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
    }

}
