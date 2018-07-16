﻿/* Copyright 2015 MongoDB Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF All KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLib.Data.Mongodb.Driver.Linq.Expressions;
using NewLib.Data.Mongodb.Driver.Linq.Expressions.ResultOperators;

namespace NewLib.Data.Mongodb.Driver.Linq.Processors.EmbeddedPipeline.MethodCallBinders
{
    internal sealed class AllBinder : IMethodCallBinder<EmbeddedPipelineBindingContext>
    {
        public static IEnumerable<MethodInfo> GetSupportedMethods()
        {
            return MethodHelper.GetEnumerableAndQueryableMethodDefinitions("All");
        }

        public Expression Bind(PipelineExpression pipeline, EmbeddedPipelineBindingContext bindingContext, MethodCallExpression node, IEnumerable<Expression> arguments)
        {
            var source = BinderHelper.BindWhere(
                pipeline,
                bindingContext,
                ExpressionHelper.GetLambda(arguments.Single()));

            return new PipelineExpression(
                source,
                pipeline.Projector,
                new AllResultOperator());
        }
    }
}
