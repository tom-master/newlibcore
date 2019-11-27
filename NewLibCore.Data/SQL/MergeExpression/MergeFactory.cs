using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.MergeExpression
{
    /// <summary>
    /// 合并工厂
    /// </summary>
    public sealed class MergeFactory
    {
        /// <summary>
        /// 创建一个默认的表达式合并对象
        /// </summary>
        /// <param name="exp"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static Merge<T> Create<T>(Expression<Func<T, Boolean>> exp = default) where T : EntityBase
        {
            return new DefaultMerge<T>(exp == null ? a => true : exp);
        }
    }
}
