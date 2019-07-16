using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.CombinationCondition
{
    /// <summary>
    /// 规约抽象类
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class Combination<T> where T : EntityBase
    {
        public Expression<Func<T, Boolean>> CombinationExpression { get; set; }

        /// <summary>
        /// 追加一个规约对象
        /// </summary>
        /// <param name="right"></param>
        /// <typeparam name="T1"></typeparam>
        /// <returns></returns>
        public Expression<Func<T, T1, Boolean>> AppendCombination<T1>(Combination<T1> right) where T1 : EntityBase
        {
            Parameter.Validate(right);
            Parameter.Validate(right.CombinationExpression);

            Expression leftBody, rightBody;
            ParameterExpression leftParameter, rightParameter;
            {
                var type = typeof(T);
                leftParameter = Expression.Parameter(type, type.GetAliasName());
                var parameterVister = new ParameterVisitor(leftParameter);
                leftBody = parameterVister.Replace(CombinationExpression.Body);
            }

            {
                var type = typeof(T1);
                rightParameter = Expression.Parameter(type, type.GetAliasName());
                var parameterVister = new ParameterVisitor(rightParameter);
                rightBody = parameterVister.Replace(right.CombinationExpression.Body);
            }

            var newExpression = Expression.AndAlso(leftBody, rightBody);
            return Expression.Lambda<Func<T, T1, Boolean>>(newExpression, leftParameter, rightParameter);
        }

        /// <summary>
        /// 隐式转换为一个表达式数
        /// </summary>
        /// <param name="combination"></param>
        /// <returns></returns>
        public static implicit operator Expression<Func<T, Boolean>>(Combination<T> combination)
        {
            Parameter.Validate(combination);
            return combination.CombinationExpression;
        }
    }
}
