using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public class WhereSegment<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal WhereSegment(SegmentManager segmentManager, ExecutionCore executionCore)
        {
            _segmentManager = segmentManager;
            _executionCore = executionCore;
        }

        public SelectSegment<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);
            return new SelectSegment<TModel>(_segmentManager, _executionCore);
        }

        public SelectSegment<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new SelectSegment<TModel>(_segmentManager, _executionCore);
        }

        public SelectSegment<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return new SelectSegment<TModel>(_segmentManager, _executionCore);
        }
    }
}
