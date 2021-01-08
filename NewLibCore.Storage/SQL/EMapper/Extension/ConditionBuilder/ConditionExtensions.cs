using System;
using System.Linq.Expressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Extension.ConditionBuilder
{
    /// <summary>
    /// 合并扩展
    /// </summary>
    public static class ConditionExtensions
    {

        /// <summary>
        /// 合并一个表示 与 的表达式对象
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <typeparam name="T"></typeparam>
        public static void And<T>(this ConditionBuilderBase<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(right);

            if (left.Filter == null)
            {
                left.Filter = right;
                return;
            }

            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetEntityBaseAliasName().AliasName);
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.Filter.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var newExpression = Expression.AndAlso(leftBody, rightBody);
            left.Filter = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }

        /// <summary>
        /// 合并一个表示 或 的表达式对象
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <typeparam name="T"></typeparam>
        public static void Or<T>(this ConditionBuilderBase<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(right);

            if (left.Filter == null)
            {
                left.Filter = right;
                return;
            }

            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetEntityBaseAliasName().AliasName);
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.Filter.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var orExpression = Expression.OrElse(leftBody, rightBody);
            left.Filter = Expression.Lambda<Func<T, Boolean>>(orExpression, internalParameter);
        }

        /// <summary>
        /// 合并一个表示 非 的表达式对象
        /// </summary>
        /// <param name="left"></param>
        /// <typeparam name="T"></typeparam>
        public static void Not<T>(this ConditionBuilderBase<T> left) where T : EntityBase
        {
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(left.Filter);

            var lambdaExpression = (LambdaExpression)left.Filter;
            var internalParameter = lambdaExpression.Parameters[0];
            var newExpression = Expression.Not(lambdaExpression.Body);
            left.Filter = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }
    }
}
