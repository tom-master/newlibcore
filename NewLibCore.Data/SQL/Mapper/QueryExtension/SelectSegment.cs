using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public class SelectSegment<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager; 

        internal SelectSegment(SegmentManager segmentManager)
        {
            _segmentManager = segmentManager;
        }

        public FinalQuery<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return new FinalQuery<TModel>(_segmentManager);
        }
    }
}
