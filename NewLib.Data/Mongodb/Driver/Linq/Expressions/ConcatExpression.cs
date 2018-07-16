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

using System.Linq.Expressions;
using NewLib.Data.Mongodb.Core.Core.Misc;

namespace NewLib.Data.Mongodb.Driver.Linq.Expressions
{
    internal sealed class ConcatExpression : ExtensionExpression, ISourcedExpression
    {
        private readonly Expression _source;
        private readonly Expression _other;

        public ConcatExpression(Expression source, Expression other)
        {
            _source = Ensure.IsNotNull(source, nameof(source));
            _other = Ensure.IsNotNull(other, nameof(other));
        }

        public Expression Other
        {
            get { return _other; }
        }

        public Expression Source
        {
            get { return _source; }
        }

        public override ExtensionExpressionType ExtensionType
        {
            get { return ExtensionExpressionType.Concat; }
        }

        public override string ToString()
        {
            return string.Format("{0}.Concat({1})", _source.ToString(), _other.ToString());
        }

        public ConcatExpression Update(Expression source, Expression other)
        {
            if (source != _source || other != _other)
            {
                return new ConcatExpression(source, other);
            }

            return this;
        }

        protected internal override Expression Accept(ExtensionExpressionVisitor visitor)
        {
            return visitor.VisitConcat(this);
        }
    }
}
