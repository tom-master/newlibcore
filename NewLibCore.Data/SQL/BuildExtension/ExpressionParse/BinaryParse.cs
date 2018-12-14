using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.BuildExtension.ExpressionParse
{
    public class BinaryParse : ExpressionVisitor
    {
        public BinaryParse() { }

        protected override Expression VisitBinary(BinaryExpression node)
        {
            return base.VisitBinary(node);
        }
    }
}
