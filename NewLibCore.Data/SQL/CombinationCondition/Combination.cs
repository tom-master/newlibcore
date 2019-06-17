﻿using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.CombinationCondition
{
    public abstract class Combination<T> where T : EntityBase
    {
        public Expression<Func<T, Boolean>> CombinationExpression { get; set; }

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

        public static implicit operator Expression<Func<T, Boolean>>(Combination<T> combination)
        {
            Parameter.Validate(combination);
            return combination.CombinationExpression;
        }
    }
}
