using System;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    /// <summary>
    /// 连接语句对象
    /// </summary>
    internal class JoinComponent : AliasComponentBase
    {
        protected internal String MainTable { get; private set; }

        protected internal JoinRelation JoinRelation { get; private set; }

        protected internal JoinComponent(Expression expression, JoinRelation joinRelation, String mainTable) : base(expression)
        {
            MainTable = mainTable;
            JoinRelation = joinRelation;
        }
    }
}