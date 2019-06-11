﻿using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.CombinationCondition
{

    public static class CombinationExtensions
    {

        public static void And<T>(this Combination<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {
            if (left.Expression == default)
            {
                left.Expression = right;
                return;
            }

            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetAliasName());
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.Expression.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var newExpression = Expression.AndAlso(leftBody, rightBody);
            left.Expression = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }

        public static void Or<T>(this Combination<T> left, Expression<Func<T, Boolean>> right) where T : EntityBase
        {

            if (left.Expression == default)
            {
                left.Expression = right;
                return;
            }

            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.GetAliasName());
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.Expression.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var orExpression = Expression.OrElse(leftBody, rightBody);
            left.Expression = Expression.Lambda<Func<T, Boolean>>(orExpression, internalParameter);
        }

        public static void Not<T>(this Combination<T> left) where T : EntityBase
        {
            var lambdaExpression = (LambdaExpression)left.Expression;
            var internalParameter = lambdaExpression.Parameters[0];
            var newExpression = Expression.Not(lambdaExpression.Body);
            left.Expression = Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
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