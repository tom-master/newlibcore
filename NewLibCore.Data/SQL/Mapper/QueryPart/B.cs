using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.QueryPart
{

    public class B<TModel> : A<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;

        internal B(SegmentManager segmentManager) : base(segmentManager)
        {
            _segmentManager = segmentManager;
        }

        public B<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public B<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public B<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public B<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public B<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public B<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }
    }
}
