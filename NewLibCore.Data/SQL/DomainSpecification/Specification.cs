using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.DomainSpecification.ConcreteSpecification;
using NewLibCore.Data.SQL.PropertyExtension;

namespace NewLibCore.Data.SQL.DomainSpecification
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

		public static implicit operator Specification<T>(Expression<Func<T, Boolean>> expression)
		{
			return new DefaultSpecification<T>(expression);
		}
	}
}
