using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.CombinationCondition
{

    internal sealed class ParameterVisitor : ExpressionVisitor
    {
        internal ParameterVisitor(ParameterExpression paramExpr)
        {
            ParameterExpression = paramExpr;
        }

        internal ParameterExpression ParameterExpression { get; private set; }

        internal Expression Replace(Expression expr) => Visit(expr);

        protected override Expression VisitParameter(ParameterExpression p) => ParameterExpression;

    }
}
