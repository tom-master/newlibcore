using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.ProcessorFactory;
using NewLibCore.Validate;
using System;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper
    {
        private readonly IOptions<EntityMapperOptions> _option;
        public EntityMapper(IOptions<EntityMapperOptions> options) { _option = options; }
        public void Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            var insert = new InsertComponent(_option);
            insert.AddModel(model);
            model.Id = insert.Execute().GetModifyRowCount();
        }

        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> filter = null) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            var update = new UpdateComponent(_option);
            if (filter != null)
            {
                var whereComponent = new WhereComponent();
                whereComponent.AddWhere(filter);
                update.AddWhereComponent(whereComponent);
            }

            update.AddModel(model);
            return update.Execute().GetModifyRowCount() > 0;
        }

        public SelectComponent Query<TModel>() where TModel : EntityBase, new()
        {
            var select = new SelectComponent(_option);
            return select.Query<TModel>();
        }
    }
}
