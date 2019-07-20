using System;

namespace NewLibCore.Data.SQL.Mapper.ExpressionStatment
{

    /// <summary>
    /// 连接语句对象
    /// </summary>
    internal class JoinStatement : Statement
    {
        protected internal String MainTable { get; set; }

        protected internal JoinType JoinType { get; set; }
    }
}