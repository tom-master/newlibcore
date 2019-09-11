using System.Collections.Generic;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public class FinalQuery<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;

        internal FinalQuery(SegmentManager segmentManager)
        {
            _segmentManager = segmentManager;
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToSingle<TModel>();
            });
        }

        public T FirstOrDefault<T>() where T : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToSingle<T>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToList<TModel>();
            });
        }

        public List<T> ToList<T>() where T : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToList<T>();
            });
        }

        private RawExecuteResult InternalExecuteSql()
        {
            Handler handler = new QueryHandler<TModel>(_segmentManager);
            return handler.GetTranslationResult().Execute();
        }
    }
}
