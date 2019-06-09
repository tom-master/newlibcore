using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.CombinationCondition.ConcreteCombinationCondition;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.CombinationCondition
{
    public abstract class Combination<T> where T : EntityBase
    {
        public Expression<Func<T, Boolean>> Expression { get; set; }

        public Expression<Func<T, Boolean>> AppendCombination(Combination<T> right)
        {
            Parameter.Validate(right);

            if (right.Expression == default)
            {
                return Expression;
            }

            Expression leftBody, rightBody;
            ParameterExpression leftParameter, rightParameter;
            {
                var type = typeof(T);
                leftParameter = System.Linq.Expressions.Expression.Parameter(type, type.GetAliasName());
                var parameterVister = new ParameterVisitor(leftParameter);
                leftBody = parameterVister.Replace(Expression.Body);
            }

            {
                var type = typeof(T);
                rightParameter = System.Linq.Expressions.Expression.Parameter(type, type.GetAliasName());
                var parameterVister = new ParameterVisitor(rightParameter);
                rightBody = parameterVister.Replace(right.Expression.Body);
            }

            var newExpression = System.Linq.Expressions.Expression.AndAlso(leftBody, rightBody);
            return System.Linq.Expressions.Expression.Lambda<Func<T, Boolean>>(newExpression, leftParameter, rightParameter);
        }

        public Expression<Func<T, T1, Boolean>> AppendCombination<T1>(Combination<T1> right) where T1 : EntityBase
        {
            Parameter.Validate(right);
            Parameter.Validate(right.Expression);

            Expression leftBody, rightBody;
            ParameterExpression leftParameter, rightParameter;
            {
                var type = typeof(T);
                leftParameter = System.Linq.Expressions.Expression.Parameter(type, type.GetAliasName());
                var parameterVister = new ParameterVisitor(leftParameter);
                leftBody = parameterVister.Replace(Expression.Body);
            }

            {
                var type = typeof(T1);
                rightParameter = System.Linq.Expressions.Expression.Parameter(type, type.GetAliasName());
                var parameterVister = new ParameterVisitor(rightParameter);
                rightBody = parameterVister.Replace(right.Expression.Body);
            }

            var newExpression = System.Linq.Expressions.Expression.AndAlso(leftBody, rightBody);
            return System.Linq.Expressions.Expression.Lambda<Func<T, T1, Boolean>>(newExpression, leftParameter, rightParameter);
        }


        public static implicit operator Expression<Func<T, Boolean>>(Combination<T> combination)
        {
            Parameter.Validate(combination);
            return combination.Expression;
        }
    }
}
