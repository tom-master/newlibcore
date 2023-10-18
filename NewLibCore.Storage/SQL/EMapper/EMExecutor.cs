using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper.Visitor;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.EMapper
{
    internal class EMExecutor<T>
    {
        private Expression _expression;

        private IOptions<EntityMapperOptions> _options;

        internal EMExecutor(Expression expression, IOptions<EntityMapperOptions> options)
        {
            _expression = expression;
            _options = options;
        }

        internal T Execute()
        {
            var sql = Translate(_expression);
            return default;
        }

        private string Translate(Expression expression)
        {
            var expressions = DisassemblyExpression((MethodCallExpression)expression);

            var rootVisitors = new List<RootVisitor>();
            foreach (var methodExpression in expressions)
            {
                switch (methodExpression.Key)
                {
                    case EMType.INNER:
                    case EMType.LEFT:
                    case EMType.RIGHT:
                        rootVisitors.Add(new JoinVisitor(methodExpression.Key, methodExpression.Value, _options));
                        break;
                    case EMType.WHERE:
                        rootVisitors.Add(new WhereVisitor(methodExpression.Key, methodExpression.Value, _options));
                        break;
                    case EMType.FROM:
                        rootVisitors.Add(new FromVisitor(methodExpression.Key, methodExpression.Value, _options));
                        break;
                    case EMType.SELECT:
                        rootVisitors.Add(new SelectVisitor(methodExpression.Key, methodExpression.Value, _options));
                        break;
                    default:
                        throw new NotSupportedException();
                }
            }
            rootVisitors.ForEach(f => f.Visit(f.Expression.Value));
            return string.Join(" ", rootVisitors.OrderBy(o => o.Order).Select(s => s.VisitResult.Sql)).Replace("  ", " ");
        }

        protected Expression VisitUnary(UnaryExpression node)
        {
            return node.Operand is LambdaExpression lambdaExpression ? lambdaExpression : (Expression)node;
        }

        private List<KeyValuePair<EMType, Expression>> DisassemblyExpression(MethodCallExpression methodCall)
        {
            var m = new List<KeyValuePair<EMType, Expression>>();
            var methodCallCopy = methodCall;

        tryL:
            m.Add(new KeyValuePair<EMType, Expression>(Enum.Parse<EMType>(methodCallCopy.Method.Name, true), VisitUnary((UnaryExpression)methodCallCopy.Arguments[1])));

            foreach (var argument in methodCallCopy.Arguments)
            {
                if (argument is MethodCallExpression innerMethodCall)
                {
                    methodCallCopy = innerMethodCall;
                    goto tryL;
                }
                else if (argument is ConstantExpression constant)
                {
                    var type = ((IQueryable)constant.Value).ElementType;
                    var parameter = Expression.Parameter(type, type.GetEntityBaseAliasName().AliasName);
                    var lamdba = Expression.Lambda(Expression.Constant(type), parameter);
                    m.Add(new KeyValuePair<EMType, Expression>(EMType.FROM, lamdba));
                }
            }
            return m;
        }
    }
}
