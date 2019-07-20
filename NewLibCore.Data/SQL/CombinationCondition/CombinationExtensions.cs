using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.CombinationCondition
{
    /// <summary>
    /// 规约扩展
    /// </summary>
    public static class CombinationExtensions
    {

        /// <summary>
        /// 表示一个 与 规约
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <typeparam name="T"></typeparam>
        public static void And<T>(this Combination<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {
            if (left.CombinationExpression == default)
            {
                left.CombinationExpression = right;
                return;
            }

            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetAliasName());
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.CombinationExpression.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var newExpression = Expression.AndAlso(leftBody, rightBody);
            left.CombinationExpression = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }

        /// <summary>
        /// 表示一个 或 规约
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <typeparam name="T"></typeparam>
        public static void Or<T>(this Combination<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {

            if (left.CombinationExpression == default)
            {
                left.CombinationExpression = right;
                return;
            }

            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetAliasName());
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.CombinationExpression.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var orExpression = Expression.OrElse(leftBody, rightBody);
            left.CombinationExpression = Expression.Lambda<Func<T, Boolean>>(orExpression, internalParameter);
        }

        /// <summary>
        /// 表示一个 非 规约
        /// </summary>
        /// <param name="left"></param>
        /// <typeparam name="T"></typeparam>
        public static void Not<T>(this Combination<T> left) where T : EntityBase
        {
            var lambdaExpression = (LambdaExpression)left.CombinationExpression;
            var internalParameter = lambdaExpression.Parameters[0];
            var newExpression = Expression.Not(lambdaExpression.Body);
            left.CombinationExpression = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }

        /// <summary>
        /// 逻辑倒序排序
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        // public static Combination<T> OrderByDescending<T>(this Combination<T> left, Expression<Func<T, Object>> right) where T : EntityBase
        // {
        //     left.AddOrderByExpression(right);
        //     return left;
        // }
    }
}
