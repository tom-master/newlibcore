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
    internal class DefaultMerge<T> : Merge<T> where T : EntityBase
    {
        internal DefaultMerge(Expression<Func<T, Boolean>> exp)
        {
            Parameter.Validate(exp);
            MergeExpression = exp;
        }
    }
}
