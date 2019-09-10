using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.QueryPart;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public sealed class QueryMapper<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;

        internal QueryMapper()
        {
            _segmentManager = MapperConfig.ServiceProvider.GetService<SegmentManager>();
        }

        public B<TModel> Query()
        {
            _segmentManager.Add<TModel>();
            return new B<TModel>(_segmentManager);
        }
    }
}
