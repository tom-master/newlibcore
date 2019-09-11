using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public class JoinSegment<TModel> : WhereSegment<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        internal JoinSegment(SegmentManager segmentManager, ExecutionCore executionCore) : base(segmentManager, executionCore)
        {
            _segmentManager = segmentManager;
        }

        public JoinSegment<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public JoinSegment<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public JoinSegment<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public JoinSegment<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public JoinSegment<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public JoinSegment<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }
    }
}
