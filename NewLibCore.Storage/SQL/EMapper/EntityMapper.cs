using System.Linq;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper
    {
        private readonly IOptions<EntityMapperOptions> _options;

        public EntityMapper(IOptions<EntityMapperOptions> options)
        {
            _options = options;
        }

        public void OpenTransaction()
        {
            _options.Value.TransactionControl.UseTransaction();
        }

        public void Rollback()
        {
            _options.Value.TransactionControl.Rollback();
        }

        public void Commit()
        {
            _options.Value.TransactionControl.Commit();
        }

        public IQueryable<TModel> Query<TModel>() where TModel : EntityBase, new()
        {
            IQueryable<TModel> queryable = new EMQueryable<TModel>(_options);
            return queryable;
        }
    }
}
