using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Extension;

namespace NewLibCore.Data.SQL.Filter
{
    /// <summary>
    /// 默认的合并查询表达式 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class DefaultFilter<T> : FilterBase<T> where T : EntityBase
    {
        internal DefaultFilter(Expression<Func<T, Boolean>> filter)
        {
            Filter = filter;
        }
    }
}
