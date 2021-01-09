using System;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Extension.ConditionBuilder
{
    /// <summary>
    /// 默认的合并查询表达式 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class DefaultCondition<T> : ConditionBuilderBase<T> where T : EntityBase
    {
        internal DefaultCondition(Expression<Func<T, Boolean>> filter)
        {
            Filter = filter;
        }
    }
}
