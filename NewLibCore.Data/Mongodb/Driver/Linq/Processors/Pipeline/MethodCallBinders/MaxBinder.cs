/* Copyright 2015 MongoDB Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

using System;
using System.Collections.Generic;
using System.Reflection;
using NewLibCore.Data.Mongodb.Bson.Serialization;
using NewLibCore.Data.Mongodb.Driver.Linq.Expressions;
using NewLibCore.Data.Mongodb.Driver.Linq.Expressions.ResultOperators;

namespace NewLibCore.Data.Mongodb.Driver.Linq.Processors.Pipeline.MethodCallBinders
{
    internal sealed class MaxBinder : SelectingResultOperatorBinderBase
    {
        public static IEnumerable<MethodInfo> GetSupportedMethods()
        {
            return MethodHelper.GetEnumerableAndQueryableMethodDefinitions("Max");
        }

        protected override ResultOperator CreateResultOperator(Type resultType, IBsonSerializer serializer)
        {
            return new MaxResultOperator(resultType, serializer);
        }

        protected override AccumulatorType GetAccumulatorType()
        {
            return AccumulatorType.Max;
        }
    }
}
