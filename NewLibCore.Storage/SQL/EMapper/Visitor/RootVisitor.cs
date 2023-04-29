using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class WrapVisitor
    {
        private List<RootVisitor> _rootVisitors;
        internal IOptions<EntityMapperOptions> Options { get; private set; }

        internal WrapVisitor(IOptions<EntityMapperOptions> options)
        {
            Options = options;
            _rootVisitors = new List<RootVisitor>();
        }

        internal void Translate(List<KeyValuePair<string, Expression>> expression)
        {

            foreach(var methodExpression in expression)
            {
                switch(methodExpression.Key)
                {
                    case "InnerJoin":
                        _rootVisitors.Add(new JoinVisitor(EMType.INNER, methodExpression.Value, Options));
                        break;
                    case "LeftJoin":
                        _rootVisitors.Add(new JoinVisitor(EMType.LEFT, methodExpression.Value, Options));
                        break;
                    case "RightJoin":
                        _rootVisitors.Add(new JoinVisitor(EMType.RIGHT, methodExpression.Value, Options));
                        break;
                    case "Where":
                        _rootVisitors.Add(new WhereVisitor(EMType.WHERE, methodExpression.Value, Options));
                        break;
                    case "From":
                        _rootVisitors.Add(new FromVisitor(EMType.FROM, methodExpression.Value, Options));
                        break;
                    case "Select":
                        _rootVisitors.Add(new SelectVisitor(EMType.COLUMN, methodExpression.Value, Options));
                        break;
                    default: throw new NotSupportedException();
                }
            }
            _rootVisitors.ForEach(f => f.Visit(f.Expression.Value));
        }

        internal string PrintSql()
        {
            return string.Join(" ", _rootVisitors.OrderBy(o => o.Order).Select(s => s.VisitResult.Sql));
        }
    }


    internal class RootVisitor: ExpressionVisitor
    {
        internal KeyValuePair<EMType, Expression> Expression { get; private set; }

        internal (EMType EMType, string Sql, List<MapperParameter> Parameters) VisitResult { get; set; }

        internal IOptions<EntityMapperOptions> Options { get; private set; }

        internal virtual int Order { get; }

        protected RootVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options)
        {
            Expression = new KeyValuePair<EMType, Expression>(eMType, expression);
            Options = options;
        }

        public override Expression Visit(Expression node)
        {
            if(node.NodeType == ExpressionType.Lambda)
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
            foreach(var item in parameters)
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
    }
}
