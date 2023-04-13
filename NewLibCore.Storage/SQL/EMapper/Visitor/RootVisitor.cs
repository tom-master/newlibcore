using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal abstract class RootVisitor: ExpressionVisitor
    {
        protected override Expression VisitUnary(UnaryExpression node)
        {
            if (node.Operand is LambdaExpression lambdaExpression)
            {
                ParseExpression(lambdaExpression);
            }
            return node;
        }

        protected abstract void ParseExpression(LambdaExpression expression);
    }
}
