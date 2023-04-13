﻿using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Component;
using NewLibCore.Storage.SQL.EMapper;
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
        private readonly IOptions<EntityMapperOptions> _options;

        public EntityMapper(IOptions<EntityMapperOptions> options, IEnumerable<IEntityMapperExecutor> entityMapperExecutors)
        {
            _options = options;
            _entityMapperExecutors = entityMapperExecutors;
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

        //private IEntityMapperExecutor GetExecutor(String componentIdentity)
        //{
        //    return _entityMapperExecutors.First(e => e.ComponentIdentity == componentIdentity);
        //}

        //public void Add<TModel>(TModel model) where TModel : EntityBase, new()
        //{
        //    Check.IfNullOrZero(model);

        //    var insertComponent = (InsertComponent)GetExecutor(nameof(InsertComponent));
        //    insertComponent.AddModel(model);
        //    model.Id = insertComponent.Execute().GetModifyRowCount();
        //}

        //public void Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> filter = null) where TModel : EntityBase, new()
        //{
        //    Check.IfNullOrZero(model);
        //    var updateComponent = (UpdateComponent)GetExecutor(nameof(UpdateComponent));
        //    if (filter != null)
        //    {
        //        updateComponent.Where(filter);
        //    }

        //    updateComponent.Model<TModel>();
        //    updateComponent.Execute();
        //}

        public IQueryable<TModel> Query<TModel>() where TModel : EntityBase, new()
        {
            IQueryable<TModel> queryable = new EMQueryable<TModel>(_options);
            return queryable;
        }
    }
}
