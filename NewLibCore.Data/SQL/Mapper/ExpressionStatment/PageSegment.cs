using System;

namespace NewLibCore.Data.SQL.Mapper.ExpressionStatment
{
    /// <summary>
    /// 分页语句对象
    /// </summary>
    internal class PaginationExpressionMapper : Statement
    {
        internal Int32 Index { get; set; }

        internal Int32 Size { get; set; }
    }
}