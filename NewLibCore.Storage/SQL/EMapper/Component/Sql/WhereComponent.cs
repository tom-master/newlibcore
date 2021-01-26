
using System;
using System.Linq.Expressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class WhereComponent : ComponentBase
    {
        internal void AddWhere(Expression filter)
        {
            Check.IfNullOrZero(filter);
            Expression = filter;
            InitAliasNameMappers(ParseToAliasNames(filter).ToArray());
        }

    }
}