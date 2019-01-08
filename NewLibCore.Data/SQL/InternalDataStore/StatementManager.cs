using NewLibCore.Data.SQL.BuildExtension;
using System;
using System.Linq.Expressions;
using System.Text;

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
            new JoinParse().Parse(expression);
        }
    }

    internal class JoinParse : ExpressionVisitor
    {
        private static readonly StringBuilder _joinBuilder = new StringBuilder();

        public void Parse(Expression expression, JoinType joinType = JoinType.Inner)
        {
            var lamdbaExp = (LambdaExpression)expression;
            var parameterName = GetAliasName(lamdbaExp.Parameters[0]);
            _joinBuilder.Append($@"{joinType.GetDescription()} {parameterName} AS {parameterName.ToLower()} ON ");
            this.Visit(expression);
        }

        private String GetAliasName(ParameterExpression parameterExpression)
        {
            return parameterExpression.Type.Name;
        }

        protected override Expression VisitLambda<T>(Expression<T> node)
        {
            return base.Visit(node.Body);
        }

        protected override Expression VisitBinary(BinaryExpression node)
        {
            this.Visit(node.Left);
            switch (node.NodeType)
            {
                case ExpressionType.AndAlso:
                    _joinBuilder.Append(" AND ");
                    break;
                case ExpressionType.Or:
                    _joinBuilder.Append(" OR");
                    break;
                case ExpressionType.Equal:
                    _joinBuilder.Append(" = ");
                    break;
                case ExpressionType.NotEqual:
                    _joinBuilder.Append(" <> ");
                    break;
                case ExpressionType.LessThan:
                    _joinBuilder.Append(" < ");
                    break;
                case ExpressionType.LessThanOrEqual:
                    _joinBuilder.Append(" <= ");
                    break;
                case ExpressionType.GreaterThan:
                    _joinBuilder.Append(" > ");
                    break;
                case ExpressionType.GreaterThanOrEqual:
                    _joinBuilder.Append(" >= ");
                    break;
                default:
                    throw new NotSupportedException();
            }
            this.Visit(node.Right);
            return node;
        }

        protected override Expression VisitMember(MemberExpression node)
        {
            var parameterName = GetAliasName((ParameterExpression)node.Expression).ToLower();
            _joinBuilder.Append($@"{parameterName}.{node.Member.Name}");
            return base.VisitMember(node);
        }
    }
}
