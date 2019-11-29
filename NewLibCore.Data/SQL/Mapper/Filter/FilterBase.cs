using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.MergeExpression
{
    /// <summary>
    /// 合并作为查询条件的表达式树
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class FilterBase<T> where T : EntityBase
    {
        internal Expression<Func<T, Boolean>> Filter { get; set; }

        /// <summary>
        /// 追加一个表达式树对象
        /// </summary>
        /// <param name="right"></param>
        /// <typeparam name="T1"></typeparam>
        /// <returns></returns>
        public Expression<Func<T, T1, Boolean>> Append<T1>(FilterBase<T1> right) where T1 : EntityBase
        {
            Parameter.Validate(right);
            Parameter.Validate(right.Filter);

            Expression leftBody, rightBody;
            ParameterExpression leftParameter, rightParameter;
            {
                var type = typeof(T);
                leftParameter = Expression.Parameter(type, type.GetTableName().AliasName);
                var parameterVister = new ParameterVisitor(leftParameter);
                leftBody = parameterVister.Replace(Filter.Body);
            }

            {
                var type = typeof(T1);
                rightParameter = Expression.Parameter(type, type.GetTableName().AliasName);
                var parameterVister = new ParameterVisitor(rightParameter);
                rightBody = parameterVister.Replace(right.Filter.Body);
            }

            var newExpression = Expression.AndAlso(leftBody, rightBody);
            return Expression.Lambda<Func<T, T1, Boolean>>(newExpression, leftParameter, rightParameter);
        }

        /// <summary>
        /// 隐式转换为一个表达式树
        /// </summary>
        /// <param name="combination"></param>
        /// <returns></returns>
        public static implicit operator Expression<Func<T, Boolean>>(FilterBase<T> combination)
        {
            Parameter.Validate(combination);
            return combination.Filter;
        }
    }
}
