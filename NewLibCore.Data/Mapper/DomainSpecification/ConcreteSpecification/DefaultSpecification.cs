using System;
using System.Linq.Expressions;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper.DomainSpecification.ConcreteSpecification
{

	/// <summary>
	/// 默认规约 
	/// </summary>
	/// <typeparam name="T"></typeparam>
	internal class DefaultSpecification<T> : Specification<T> where T : PropertyMonitor, new()
	{
		public override Expression<Func<T, Boolean>> Expression { get; internal set; }

		public DefaultSpecification(Expression<Func<T, Boolean>> expression)
		{
			Expression = expression;
		}

		public DefaultSpecification() : this(T => true) { }

	}
}
