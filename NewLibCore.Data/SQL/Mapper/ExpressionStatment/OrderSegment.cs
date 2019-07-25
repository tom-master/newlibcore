
namespace NewLibCore.Data.SQL.Mapper.ExpressionStatment
{

    /// <summary>
    /// 排序语句对象
    /// </summary>
    internal class OrderStatement : Statement
    {
        protected internal OrderByType OrderBy { get; set; }
    }

}