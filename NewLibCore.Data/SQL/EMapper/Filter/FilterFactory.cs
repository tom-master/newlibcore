using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Extension;

namespace NewLibCore.Data.SQL.Filter
{
    /// <summary>
    /// 合并工厂
    /// </summary>
    public sealed class FilterFactory
    {
        /// <summary>
        /// 创建一个默认的表达式合并对象
        /// </summary>
        /// <param name="exp"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static FilterBase<T> Create<T>(Expression<Func<T, Boolean>> filter = null) where T : EntityBase
        {
            if (filter == null)
            {
                filter = (t) => true;
            }
            return new DefaultFilter<T>(filter);
        }
    }
}
