using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.CombineCondition.ConcreteCombineCondition
{
    /// <summary>
    /// 默认规约工厂
    /// </summary>
    public sealed class CombineFactory
    {
        public static Combine<T> Create<T>(Expression<Func<T, Boolean>> expression = null) where T : EntityBase
        {
            return expression == null ? new DefaultCombine<T>() : new DefaultCombine<T>(expression);
        }
    }
}
