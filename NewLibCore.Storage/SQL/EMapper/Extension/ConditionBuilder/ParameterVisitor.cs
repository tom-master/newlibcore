using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Extension.ConditionBuilder
{

    /// <summary>
    /// 表达式树 参数访问类
    /// </summary>
    internal sealed class ParameterVisitor : ExpressionVisitor
    {
        internal ParameterVisitor(ParameterExpression paramExpr)
        {
            ParameterExpression = paramExpr;
        }

        internal ParameterExpression ParameterExpression { get; private set; }

        /// <summary>
        /// 替换表达式
        /// </summary>
        /// <param name="expr"></param>
        /// <returns></returns>
        internal Expression Replace(Expression expr) => Visit(expr);

        protected override Expression VisitParameter(ParameterExpression p) => ParameterExpression;

    }
}
