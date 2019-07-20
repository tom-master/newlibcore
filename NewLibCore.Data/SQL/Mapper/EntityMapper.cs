using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Execute; 
using NewLibCore.Data.SQL.Mapper.OperationProvider;
using NewLibCore.Data.SQL.Mapper.OperationProvider.Imp;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    public sealed class EntityMapper : IDisposable
    {
        private readonly ExecuteCore _executeCore;

        public EntityMapper()
        {
            _executeCore = new ExecuteCore();
        }

        public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);
            return new AddMapper<TModel>(_executeCore).Add(model);
        }

        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);
            Parameter.Validate(expression);
            return new ModifyMapper<TModel>(_executeCore).Update(model, expression);
        }

        public ISearchMapper<TModel> Select<TModel>(Expression<Func<TModel, dynamic>> fields = null) where TModel : EntityBase, new()
        {
            return new SearchMapper<TModel>(_executeCore).Select(fields);
        }

        public ISearchMapper<TModel> Select<TModel, T>(Expression<Func<TModel, T, dynamic>> fields = null) where TModel : EntityBase, new()
        where T : EntityBase, new()
        {
            return new SearchMapper<TModel>(_executeCore).Select(fields);
        }

        public List<TModel> ExecuteToList<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            Parameter.Validate(sql);
            return new RawExecutor(_executeCore).ToList<TModel>(sql, parameters);
        }

        public TModel ExecuteToSingle<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            Parameter.Validate(sql);
            return new RawExecutor(_executeCore).ToSingle<TModel>(sql, parameters);
        }

        public void OpenTransaction()
        {
            _executeCore.OpenTransaction();
        }

        public void Commit()
        {
            _executeCore.Commit();
        }

        public void Rollback()
        {
            _executeCore.Rollback();
        }

        public void Dispose()
        {
            _executeCore.Dispose();
        }
    }
}
