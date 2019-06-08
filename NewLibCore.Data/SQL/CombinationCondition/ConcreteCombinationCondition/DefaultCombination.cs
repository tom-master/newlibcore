using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.CombinationCondition.ConcreteCombinationCondition
{
    /// <summary>
    /// 默认规约 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class DefaultCombination<T> : Combination<T> where T : EntityBase
    {
        public override Expression<Func<T, Boolean>> Expression { get; internal set; }

        public sealed override Expression<Func<T, Object>> OrderBy
        {
            get; protected set;
        }

        public DefaultCombination(Expression<Func<T, Boolean>> expression)
        {
            Expression = expression;
            OrderBy = t => t.Id;
        }

        public DefaultCombination() : this(T => true) { }

        public override void AddOrderByExpression(Expression<Func<T, Object>> expression)
        {
            OrderBy = expression;
        }

        public override void ResetOrderByExpressions() => OrderBy = null;


    }
}
