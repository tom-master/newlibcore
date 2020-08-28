using System;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Extension.Filter
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
