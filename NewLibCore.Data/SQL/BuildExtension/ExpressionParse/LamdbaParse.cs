using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.BuildExtension.ExpressionParse
{
    public class LamdbaParse : ExpressionVisitor
    {
        public LamdbaParse() { }

        protected override Expression VisitLambda<T>(Expression<T> node)
        {
            if (node.Body is BinaryExpression)
            {
               // InternalBuildWhere((BinaryExpression)node.Body);
            }

            return base.VisitLambda(node);
        }
    }
}
