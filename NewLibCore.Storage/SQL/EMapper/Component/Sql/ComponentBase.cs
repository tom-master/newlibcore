using System;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class ComponentBase
    {
        protected internal Expression Expression { get; private set; }
        protected internal ComponentBase(Expression expression)
        {
            Expression = expression;
        }
    }

}