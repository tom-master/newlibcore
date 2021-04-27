using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.Extension;
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
        private readonly InsertComponent _insertComponent;
        private readonly UpdateComponent _updateComponent;
        private readonly SelectWrapper _selectWrapper;
        public EntityMapper(InsertComponent insertComponent, UpdateComponent updateComponent, SelectWrapper selectWrapper)
        {
            _insertComponent = insertComponent;
            _updateComponent = updateComponent;
            _selectWrapper = selectWrapper;
        }

        public void Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            _insertComponent.AddModel(model);
            model.Id = _insertComponent.Execute().GetModifyRowCount();
        }

        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> filter = null) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);

            if (filter != null)
            {
                var whereComponent = new WhereComponent();
                whereComponent.AddWhere(filter);
                _updateComponent.AddWhereComponent(whereComponent);
            }

            _updateComponent.AddModel(model);
            return _updateComponent.Execute().GetModifyRowCount() > 0;
        }

        public SelectWrapper Query<TModel>() where TModel : EntityBase, new()
        {
            return _selectWrapper.Query<TModel>();
        }
    }
}
