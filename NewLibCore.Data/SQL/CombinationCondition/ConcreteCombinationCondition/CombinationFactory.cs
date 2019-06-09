using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.CombinationCondition.ConcreteCombinationCondition
{
    /// <summary>
    /// 默认规约工厂
    /// </summary>
    public sealed class CombinationFactory
    {
        public static Combination<T> Create<T>(Expression<Func<T, Boolean>> exp = default) where T : EntityBase
        {
            return new DefaultCombination<T>(exp == default ? a => true : exp);
        }
    }
}
