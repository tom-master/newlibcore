using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.Builder
{
    internal class SelectBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Queue<ExpressionModel> _keyValuePairs = new Queue<ExpressionModel>();


        internal SelectBuilder(TModel model) : base(model)
        {

        }

        protected internal override BuildEntry<TModel> Build()
        {
            while (_keyValuePairs.Count != 0)
            {
                var expressionModel = _keyValuePairs.Dequeue();
            }
            return null;
        }

        internal void Join(ExpressionModel expression)
        {
            _keyValuePairs.Enqueue(expression);
        }
    }

    internal class ExpressionModel
    {
        internal JoinType JoinType { get; private set; }

        internal Expression Expression { get; private set; }

        internal ExpressionModel(Expression expression) : this(expression, JoinType.INNER)
        {

        }

        internal ExpressionModel(Expression expression, JoinType joinType)
        {
            Expression = expression;
            JoinType = joinType;
        }
    }
}
