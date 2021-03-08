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

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="model">要新增的对象</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);

            return RunDiagnosis.Watch(() =>
            {
                model.Id = _insertComponent.Execute(model).GetModifyRowCount();
                return model;
            });
        }

        /// <summary>
        /// 修改
        /// </summary>
        /// <param name="model">要修改的对象</param>
        /// <param name="expression">查询条件</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            Check.IfNullOrZero(expression);

            return RunDiagnosis.Watch(() =>
            {
                var whereComponent = new WhereComponent();
                whereComponent.AddWhere(expression);

                _updateComponent.AddModel(model);
                _updateComponent.AddWhereComponent(whereComponent);
                return _updateComponent.Execute().GetModifyRowCount() > 0;
            });
        }
    }
}
