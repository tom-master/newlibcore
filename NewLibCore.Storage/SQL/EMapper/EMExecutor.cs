using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper.Visitor;

namespace NewLibCore.Storage.SQL.EMapper
{
    internal class EMExecutor<T>
    {
        private Expression _expression;

        private List<KeyValuePair<string, Expression>> _methodExpressions = new List<KeyValuePair<string, Expression>>();

        private IOptions<EntityMapperOptions> _options;

        internal EMExecutor(Expression expression, IOptions<EntityMapperOptions> options)
        {
            _expression = expression;
            _options = options;
        }

        internal T Execute()
        {
            GetExpressionMethod((MethodCallExpression)_expression);
            var visitor = new WrapVisitor(_options);
            visitor.Translate(_methodExpressions);
            var sql = visitor.PrintSql();
            return default;
        }

        protected Expression VisitUnary(UnaryExpression node)
        {
            if(node.Operand is LambdaExpression lambdaExpression)
            {
                return lambdaExpression;
            }
            return node;
        }

        private void GetExpressionMethod(MethodCallExpression methodCallExpression)
        {
            var methodName = methodCallExpression.Method.Name;
            var expression = methodCallExpression.Arguments[1];

            _methodExpressions.Add(new KeyValuePair<string, Expression>(methodName, VisitUnary((UnaryExpression)expression)));
            foreach(var item in methodCallExpression.Arguments)
            {
                if(item is MethodCallExpression callExpression)
                {
                    GetExpressionMethod(callExpression);
                }
                else if(item is ConstantExpression constantExpression)
                {
                    var ss = ((IQueryable)constantExpression.Value).ElementType;
                    var p1 = Expression.Parameter(ss, "a");
                    var c1 = Expression.Constant(ss);
                    var e1 = Expression.Lambda(c1, p1);
                    _methodExpressions.Add(new KeyValuePair<string, Expression>("From", e1));
                }
            }
        }
    }
}
