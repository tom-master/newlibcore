using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.EMapper.Component.SqlComponent;
using NewLibCore.Validate;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper
    {
        private readonly IEnumerable<IEntityMapperExecutor> _entityMapperExecutors;
        private readonly EntityMapperOptions _options;

        public EntityMapper(IOptions<EntityMapperOptions> options, IEnumerable<IEntityMapperExecutor> entityMapperExecutors)
        {
            _options = options.Value;
            _entityMapperExecutors = entityMapperExecutors;
        }

        public void OpenTransaction()
        {
            _options.TransactionControl.UseTransaction();
        }

        public void Rollback()
        {
            _options.TransactionControl.Rollback();
        }

        public void Commit()
        {
            _options.TransactionControl.Commit();
        }

        private IEntityMapperExecutor GetExecutor(String componentIdentity)
        {
            return _entityMapperExecutors.First(e => e.ComponentIdentity == componentIdentity);
        }

        public void Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);

            var insertComponent = (InsertComponent)GetExecutor(nameof(InsertComponent));
            insertComponent.AddModel(model);
            model.Id = insertComponent.Execute().GetModifyRowCount();
        }

        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> filter = null) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            var updateComponent = (UpdateComponent)GetExecutor(nameof(UpdateComponent));
            if (filter != null)
            {
                var whereComponent = new WhereComponent();
                whereComponent.AddWhere(filter);
                updateComponent.AddWhereComponent(whereComponent);
            }

            updateComponent.AddModel(model);
            return updateComponent.Execute().GetModifyRowCount() > 0;
        }

        public SelectComponent Query<TModel>() where TModel : EntityBase, new()
        {
            var selectComponent = (SelectComponent)GetExecutor(nameof(SelectComponent));
            return selectComponent.Query<TModel>();
        }
    }
}
