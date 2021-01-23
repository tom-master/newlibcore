using System;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    /// <summary>
    /// 简单语句对象
    /// </summary>
    internal class SimpleComponent : AliasComponentBase
    {
        protected internal SimpleComponent(Expression expression) : base(expression) { }
    }
}