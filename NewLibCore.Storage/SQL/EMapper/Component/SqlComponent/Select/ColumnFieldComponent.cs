using System;
using System.Linq.Expressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{

    internal class ColumnFieldComponent : ComponentBase
    {
        internal void AddColumnField(Expression selector)
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }
    }
}