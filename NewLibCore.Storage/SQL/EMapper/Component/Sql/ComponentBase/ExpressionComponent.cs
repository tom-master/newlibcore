
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.Component.Sql.ComponentBase
{
    internal abstract class ExpressionComponent : AliasNameComponent
    {
        internal Expression Expression { get; set; }

        protected List<KeyValuePair<String, String>> ParseToAliasNames(Expression expression)
        {
            var list = new List<KeyValuePair<String, String>>();
            var parameters = ((LambdaExpression)expression).Parameters;
            foreach (var item in parameters)
            {
                var (TableName, AliasName) = item.Type.GetEntityBaseAliasName();
                list.Add(new KeyValuePair<String, String>(TableName, AliasName));
            }
            return list.ToList();
        }

    }
}