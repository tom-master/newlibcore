using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.SQL;
using NewLibCore.UnitTest.Entitys.Agent;

namespace NewLibCore.UnitTest
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            Create<User>("Name+LoginPassword+Id", typeof(User), new User("wasd", "wasd", null));
        }


        public Expression<Func<T, bool>> Create<T>(string fields, Type dataType, object parameterInstance)
        {
            if (string.IsNullOrEmpty(fields))
            {
                throw new Exception();
            }

            if (dataType == null)
            {
                throw new Exception();
            }

            if (parameterInstance == null)
            {
                throw new Exception();
            }
            var expressions = new List<Expression<Func<T, bool>>>();
            var fieldArray = fields.Split(new[] { "+" }, StringSplitOptions.RemoveEmptyEntries);
            foreach (var item in fieldArray)
            {
                var dataPropertyInfo = dataType.GetProperty(item);
                if (dataPropertyInfo == null)
                {
                    continue;
                }

                var parameterPropertyInfo = parameterInstance.GetType().GetProperty(item);
                if (parameterPropertyInfo == null)
                {
                    continue;
                }

                var leftParameter = Expression.Parameter(dataType, dataType.Name.ToLower());
                var left = Expression.Property(leftParameter, dataPropertyInfo);
                var value = parameterPropertyInfo.GetValue(parameterInstance);
                var right = Expression.Constant(value);
                var body = Expression.Equal(left, right);
                expressions.Add(Expression.Lambda<Func<T, Boolean>>(body, leftParameter));
            }

            var expression = expressions[0];
            for (int i = 1; i < expressions.Count; i++)
            {
                expression = expression.And(expressions[i]);
            }
            return expression;
        }
    }

    public static class AndExtension
    {
        public static Expression<Func<T, Boolean>> And<T>(this Expression<Func<T, Boolean>> left, Expression<Func<T, Boolean>> right)
        {
            var type = typeof(T);
            var internalParameter = Expression.Parameter(type, type.Name.ToLower());
            var parameterVister = new ParameterVisitor(internalParameter);
            var leftBody = parameterVister.Replace(left.Body);
            var rightBody = parameterVister.Replace(right.Body);
            var newExpression = Expression.AndAlso(leftBody, rightBody);
            return Expression.Lambda<Func<T, Boolean>>(newExpression, internalParameter);
        }
    }

    internal sealed class ParameterVisitor : ExpressionVisitor
    {
        internal ParameterVisitor(ParameterExpression paramExpr)
        {
            ParameterExpression = paramExpr;
        }

        internal ParameterExpression ParameterExpression { get; private set; }

        /// <summary>
        /// 替换表达式
        /// </summary>
        /// <param name="expr"></param>
        /// <returns></returns>
        internal Expression Replace(Expression expr) => Visit(expr);

        protected override Expression VisitParameter(ParameterExpression p) => ParameterExpression;
    }
}
