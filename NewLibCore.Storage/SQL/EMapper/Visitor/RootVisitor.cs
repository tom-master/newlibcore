using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{

    internal class RootVisitor: ExpressionVisitor
    {
        internal KeyValuePair<EMType, Expression> Expression { get; private set; }

        internal (EMType EMType, string Sql, List<MapperParameter> Parameters) VisitResult { get; set; }

        internal virtual int Order { get; } = -1;

        protected IOptions<EntityMapperOptions> Options { get; private set; }

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
    }
}
