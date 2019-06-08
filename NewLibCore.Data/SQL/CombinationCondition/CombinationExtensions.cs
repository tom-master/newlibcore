using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.CombinationCondition
{
    /// <summary>
    /// 规约模式扩展
    /// </summary>
    public static class CombinationExtensions
    {
        /// <summary>
        /// 逻辑和
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        public static void And<T>(this Combination<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {
            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetAliasName());
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.Expression.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var newExpression = Expression.AndAlso(leftBody, rightBody);
            left.Expression = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }

        /// <summary>
        /// 逻辑或
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        public static void Or<T>(this Combination<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {
            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetAliasName());
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.Expression.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var orExpression = Expression.OrElse(leftBody, rightBody);
            left.Expression = Expression.Lambda<Func<T, Boolean>>(orExpression, internalParameter);
        }

        /// <summary>
        /// 逻辑非
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="left"></param>
        /// <returns></returns>
        public static void Not<T>(this Combination<T> left) where T : EntityBase
        {
            var internalParameter = left.Expression.Parameters[0];
            var newExpression = Expression.Not(left.Expression.Body);
            left.Expression = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }

        /// <summary>
        /// 逻辑倒序排序
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        public static Combination<T> OrderByDescending<T>(this Combination<T> left, Expression<Func<T, Object>> right) where T : EntityBase
        {
            left.AddOrderByExpression(right);
            return left;
        }
    }
}
