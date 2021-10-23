
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component
{
    internal abstract class ComponentBase
    {
        protected internal IList<KeyValuePair<String, String>> AliasNameMappers { get; } = new List<KeyValuePair<String, String>>();

        internal Expression Expression { get; private set; }

        internal void AddExpression(Expression expression)
        {
            Check.IfNullOrZero(expression);
            Expression = expression;

            ParseToAliasNames(Expression);
        }

        protected void ParseToAliasNames(Expression expression)
        {
            var parameters = ((LambdaExpression)expression).Parameters;
            foreach (var item in parameters)
            {
                var (tableName, aliasName) = item.Type.GetEntityBaseAliasName();
                if (AliasNameMappers.Any(w => w.Key == tableName || w.Value == aliasName))
                {
                    continue;
                }
                AliasNameMappers.Add(new KeyValuePair<String, String>(tableName, aliasName));
            }
        }
    }
}