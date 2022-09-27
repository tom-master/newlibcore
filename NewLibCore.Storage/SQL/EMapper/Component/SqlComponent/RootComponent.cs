
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component
{
    internal class RootComponent
    {
        protected internal IList<KeyValuePair<PredicateType, Expression>> PredicateExpressions { get; } = new List<KeyValuePair<PredicateType, Expression>>();

        internal void AddExpression(Expression expression, PredicateType predicateType)
        {
            Check.IfNullOrZero(expression);
            PredicateExpressions.Add(new KeyValuePair<PredicateType, Expression>(predicateType, expression));
        }

        internal KeyValuePair<string, string> GetMainTable()
        {
            var fromExpression = PredicateExpressions.Where(w => w.Key == PredicateType.FROM).FirstOrDefault();
            return ExtractAliasNames(fromExpression.Value).FirstOrDefault();
        }

        public List<KeyValuePair<string, string>> ExtractAliasNames(Expression expression)
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
    }
}