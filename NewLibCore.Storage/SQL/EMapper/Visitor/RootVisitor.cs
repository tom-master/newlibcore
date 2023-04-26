using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class RootVisitor : ExpressionVisitor
    {
        internal KeyValuePair<EMType, Expression> Expression { get; private set; }

        protected (EMType EMType, string Sql, List<MapperParameter> Parameters) VisitResult { get; set; }

        internal IOptions<EntityMapperOptions> Options { get; private set; }

        protected RootVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options)
        {
            Expression = new KeyValuePair<EMType, Expression>(eMType, expression);
            Options = options;
        }

        public override Expression Visit(Expression node)
        {
            if (node.NodeType == ExpressionType.Lambda)
            {
                var m = typeof(ExpressionVisitor).GetMethod("VisitLambda", BindingFlags.NonPublic | BindingFlags.Instance);
                var f = m.MakeGenericMethod(node.Type);
                f.Invoke(this, new object[] { node });
            }
            return node;
        }

        protected override Expression VisitLambda<T>(Expression<T> node)
        {
            ParseExpression(node);
            return node;
        }

        protected List<KeyValuePair<string, string>> ExtractAliasNames(Expression expression)
        {
            var parameters = ((LambdaExpression)expression).Parameters;
            var result = new List<KeyValuePair<string, string>>();
            foreach (var item in parameters)
            {
                var (tableName, aliasName) = item.Type.GetEntityBaseAliasName();
                result.Add(new KeyValuePair<string, string>(tableName, aliasName));
            }
            return result.Distinct().ToList();
        }

        protected virtual void ParseExpression(LambdaExpression expression)
        {
            throw new NotImplementedException();
        }

        internal void TranslateToSql(List<KeyValuePair<string, Expression>> expression)
        {
            var rootVisitors = new List<RootVisitor>();
            foreach (var methodExpression in expression)
            {
                switch (methodExpression.Key)
                {
                    case "InnerJoin":
                        rootVisitors.Add(new JoinVisitor(EMType.INNER, methodExpression.Value, Options));
                        break;
                    case "LeftJoin":
                        rootVisitors.Add(new JoinVisitor(EMType.LEFT, methodExpression.Value, Options));
                        break;
                    case "RightJoin":
                        rootVisitors.Add(new JoinVisitor(EMType.RIGHT, methodExpression.Value, Options));
                        break;
                    case "Where":
                        rootVisitors.Add(new WhereVisitor(EMType.WHERE, methodExpression.Value, Options));
                        break;
                    case "From":
                        rootVisitors.Add(new FromVisitor(EMType.FROM, methodExpression.Value, Options));
                        break;
                    case "Select":
                        rootVisitors.Add(new SelectVisitor(EMType.COLUMN, methodExpression.Value, Options));
                        break;
                    default: throw new NotSupportedException();
                }
            }
            rootVisitors.ForEach(f => f.Visit(f.Expression.Value));
        }
    }
}
