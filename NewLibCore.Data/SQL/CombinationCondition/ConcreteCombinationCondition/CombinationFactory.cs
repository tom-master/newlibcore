using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.CombinationCondition.ConcreteCombinationCondition
{
    /// <summary>
    /// 默认规约工厂
    /// </summary>
    public sealed class CombinationFactory
    {
        /// <summary>
        /// 创建一个默认的规约对象
        /// </summary>
        /// <param name="exp"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static Combination<T> Create<T>(Expression<Func<T, Boolean>> exp = default) where T : EntityBase
        {
            return new DefaultCombination<T>(exp == default ? a => true : exp);
        }
    }
}
