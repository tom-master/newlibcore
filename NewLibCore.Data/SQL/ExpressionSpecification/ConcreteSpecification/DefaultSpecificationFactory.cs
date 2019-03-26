﻿using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.MapperExtension;

namespace NewLibCore.Data.SQL.ExpressionSpecification.ConcreteSpecification
{
    /// <summary>
    /// 默认规约工厂
    /// </summary>
    public sealed class DefaultSpecificationFactory
    {
        public static Specification<T> Create<T>(Expression<Func<T, Boolean>> expression = null) where T:DomainModelBase
        {
            return expression == null ? new DefaultSpecification<T>() : new DefaultSpecification<T>(expression);
        }
    }
}