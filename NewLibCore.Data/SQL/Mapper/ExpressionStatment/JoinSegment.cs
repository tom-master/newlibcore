using System;

namespace NewLibCore.Data.SQL.Mapper.ExpressionStatment
{

    /// <summary>
    /// 连接语句对象
    /// </summary>
    internal class JoinExpressionMapper : Statement
    {
        protected internal String MainTable { get; set; }

        protected internal JoinRelation JoinRelation { get; set; }
    }
}