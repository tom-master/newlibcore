using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class JoinManager
    {
        private readonly String _aliasName;

        private readonly JoinType _joinType;

        private readonly Expression _joinExpression;

        private static IList<JoinManager> _joins = new List<JoinManager>();

        private JoinManager(Expression expression, JoinType joinType)
        {
            _joinExpression = expression;
            _joinType = joinType;
        }

        public static void RegisterJoin(Expression joinExpression, JoinType joinType)
        {
            _joins.Add(new JoinManager(joinExpression, joinType));
        }
    }

    public enum JoinType
    {
        Inner = 1,

        Left = 2,

        Right = 3
    }
}
