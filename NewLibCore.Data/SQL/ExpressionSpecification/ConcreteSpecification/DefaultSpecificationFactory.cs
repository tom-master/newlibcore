using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.ExpressionSpecification.Factory;

namespace NewLibCore.Data.SQL.ExpressionSpecification.ConcreteSpecification
{
	/// <summary>
	/// 默认规约工厂
	/// </summary>
	public sealed class DefaultSpecificationFactory : SpecificationFactory
	{
		public override Specification<T> Create<T>(Expression<Func<T, Boolean>> expression = null)
		{
			return expression == null ? new DefaultSpecification<T>() : new DefaultSpecification<T>(expression);
		}
	}
}
