using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Google.Protobuf.WellKnownTypes;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Component;

namespace NewLibCore.Storage.SQL.EMapper
{
    internal class EMExecutor<T>: PredicateExpressionTranslator
    {
        private Expression _expression;

        private List<KeyValuePair<string, Expression>> _methodExpressions = new List<KeyValuePair<string, Expression>>();

        internal EMExecutor(Expression expression, IOptions<EntityMapperOptions> options) : base(options)
        {
            _expression = expression;
        }

        internal T Execute()
        {
            var m = ((MethodCallExpression)_expression);
            GetExpressionMethod(m);
            RootComponent rootComponent = new RootComponent();
            foreach (var methodExpression in _methodExpressions)
            {
                switch (methodExpression.Key)
                {
                    case "InnerJoin":
                        rootComponent.AddExpression(methodExpression.Value, PredicateType.INNER);
                        break;
                    case "LeftJoin":
                        rootComponent.AddExpression(methodExpression.Value, PredicateType.LEFT);
                        break;
                    case "RightJoin":
                        rootComponent.AddExpression(methodExpression.Value, PredicateType.RIGHT);
                        break;
                    case "Where":
                        rootComponent.AddExpression(methodExpression.Value, PredicateType.WHERE);
                        break;
                    case "From":
                        rootComponent.AddExpression(methodExpression.Value, PredicateType.FROM);
                        break;
                    case "Select":
                        rootComponent.AddExpression(methodExpression.Value, PredicateType.COLUMN);
                        break;
                    default: throw new NotSupportedException();
                }
            }
            
            Translate(rootComponent);
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
