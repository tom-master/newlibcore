using System;
using System.Linq.Expressions;
using NewLibCore.Data.Mapper.DomainSpecification.Factory;

namespace NewLibCore.Data.Mapper.DomainSpecification.ConcreteSpecification
{
	/// <summary>
	/// 默认规约工厂
	/// </summary>
	public sealed class DefaultSpecificationFactory : SpecificationFactory
	{
		public override Specification<T> Create<T>(Expression<Func<T, Boolean>> expression = default(Expression<Func<T, Boolean>>))
		{
			return expression == default(Expression<Func<T, Boolean>>) ? new DefaultSpecification<T>() : new DefaultSpecification<T>(expression);
		}
	}
}
