using System;
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
            List<RootVisitor> rootVisitors = new List<RootVisitor>();
            foreach (var methodExpression in _methodExpressions)
            {
                switch (methodExpression.Key)
                {
                    case "InnerJoin":
                        rootVisitors.Add(new JoinVisitor(EMType.INNER, methodExpression.Value, _options));
                        break;
                    case "LeftJoin":
                        rootVisitors.Add(new JoinVisitor(EMType.LEFT, methodExpression.Value, _options));
                        break;
                    case "RightJoin":
                        rootVisitors.Add(new JoinVisitor(EMType.RIGHT, methodExpression.Value, _options));
                        break;
                    case "Where":
                        rootVisitors.Add(new WhereVisitor(EMType.WHERE, methodExpression.Value, _options));
                        break;
                    case "From":
                        rootVisitors.Add(new FromVisitor(EMType.FROM, methodExpression.Value, _options));
                        break;
                    case "Select":
                        rootVisitors.Add(new SelectVisitor(EMType.COLUMN, methodExpression.Value, _options));
                        break;
                    default: throw new NotSupportedException();
                }
            }
            rootVisitors.ForEach(f => f.Visit(f.Expression.Value));
            return default;
        }

        protected Expression VisitUnary(UnaryExpression node)
        {
            if (node.Operand is LambdaExpression lambdaExpression)
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
            foreach (var item in methodCallExpression.Arguments)
            {
                if (item is MethodCallExpression callExpression)
                {
                    GetExpressionMethod(callExpression);
                }
                else if (item is ConstantExpression constantExpression)
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
