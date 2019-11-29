using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.MergeExpression
{
    /// <summary>
    /// 默认的合并查询表达式 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class DefaultFilter<T> : FilterBase<T> where T : EntityBase
    {
        internal DefaultFilter(Expression<Func<T, Boolean>> filter)
        {
            Parameter.Validate(filter);
            Filter = filter;
        }
    }
}
