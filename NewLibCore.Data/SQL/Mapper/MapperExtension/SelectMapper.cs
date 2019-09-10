using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.QueryPart;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public sealed class QueryMapper<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly ExecutionCore _executionCore;

        internal QueryMapper(ExecutionCore executionCore)
        {
            _executionCore = executionCore;
            _segmentManager = MapperConfig.ServiceProvider.GetService<SegmentManager>();
        }

        public B<TModel> Query()
        {
            _segmentManager.Add<TModel>();
            return new B<TModel>(_segmentManager, _executionCore);
        }
    }
}
