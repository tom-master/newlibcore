using System;
using System.Linq.Expressions;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper.DomainSpecification
{
    /// <summary>
    /// 规约抽象类
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class Specification<T> where T : PropertyMonitor, new()
	{
        /// <summary>
        /// 查询表达式
        /// </summary>
        public abstract Expression<Func<T, Boolean>> Expression { get; internal set; }

        //public static explicit operator Specification<T>(Expression<Func<T, Boolean>> expression)
        //{
        //    return new DefaultSpecification<T>(expression);
        //}
    }
}
