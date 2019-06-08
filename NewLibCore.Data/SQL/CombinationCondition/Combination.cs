using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.CombinationCondition
{
    /// <summary>
    /// 规约抽象类
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class Combination<T> where T : EntityBase
    {
        /// <summary>
        /// 查询表达式
        /// </summary>
        public abstract Expression<Func<T, Boolean>> Expression { get; internal set; }

        /// <summary>
        /// 排序表达式集合
        /// </summary>
        public abstract Expression<Func<T, Object>> OrderBy { get; protected set; }

        /// <summary>
        /// 添加一个排序表达式
        /// </summary>
        /// <param name="expression"></param>
        public abstract void AddOrderByExpression(Expression<Func<T, Object>> expression);

        /// <summary>
        /// 重置排序表达式集合
        /// </summary>
        public abstract void ResetOrderByExpressions();

        //public static explicit operator Specification<T>(Expression<Func<T, Boolean>> expression)
        //{
        //    return new DefaultSpecification<T>(expression);
        //}
    }
}
