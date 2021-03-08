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
            model.Id = _insertComponent.Execute(model).GetModifyRowCount();
 
           
        }

        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            Check.IfNullOrZero(expression);

            var whereComponent = new WhereComponent();
            whereComponent.AddWhere(expression);

            _updateComponent.AddModel(model);
            _updateComponent.AddWhereComponent(whereComponent);
            return _updateComponent.Execute().GetModifyRowCount() > 0;
          
        }

        public SelectWrapper Query<TModel>() where TModel : EntityBase, new()
        {
            return _selectWrapper.Query<TModel>();
        }
    }
}
