using System;
using System.Linq.Expressions;

namespace NewLibCore.Data.Mapper.DomainSpecification.ConcreteSpecification
{

    /// <summary>
    /// 默认规约 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class DefaultSpecification<T> : Specification<T> where T : PropertyMonitor, new()
	{
        public  override Expression<Func<T, Boolean>> Expression { get; internal set; }

        public sealed override Expression<Func<T, Object>> OrderBy
        {
            get; protected set;
        }

        public DefaultSpecification(Expression<Func<T, Boolean>> expression)
        {
            Expression = expression;

            OrderBy = t => t.Id;
        }

        public DefaultSpecification() : this(T => true){}

        public override void AddOrderByExpression(Expression<Func<T, Object>> expression)
        {
            OrderBy = expression;
        }

        public override void ResetOrderByExpressions() => OrderBy = null;

   
    }
}
