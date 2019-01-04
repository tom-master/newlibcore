using NewLibCore.Data.SQL.BuildExtension;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class StatementManager
    {
        private readonly JoinType _joinType;

        public StatementManager(Expression expression, JoinType joinType = JoinType.Inner)
        {
            GetEntityAliasName(expression);
        }

        private void GetEntityAliasName(Expression expression)
        {
            new JoinParse().Visit(expression);
        }
    }

    internal class JoinParse : ExpressionVisitor
    {
        public override Expression Visit(Expression node)
        {
            return base.Visit(node);
        }
    }
}
