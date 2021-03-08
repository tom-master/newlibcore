using System;
using System.Linq.Expressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{

    internal class SelectComponent : ComponentBase
    {
        internal void AddSelect(Expression selector)
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }
      
    }
}