using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.InternalExecute;
using NewLibCore.Data.SQL.InternalTranslation;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.DataMapper
{
    public sealed class EntityMapper : IDisposable
    {
        private readonly StatementStore _statementStore;
        private readonly ExecuteContext _executeContext;

        public EntityMapper()
        {
            _executeContext = new ExecuteContext();
            _statementStore = new StatementStore();
        }

        public void OpenTransaction()
        {
            _executeContext.OpenTransaction();
        }

        public void Commit()
        {
            _executeContext.Commit();
        }

        public void Rollback()
        {
            _executeContext.Rollback();
        }

        public TModel Add<TModel>(TModel model) where TModel : DomainModelBase, new()
        {
            BuilderBase<TModel> builder = new AddBuilder<TModel>(model, true);
            var translationResult = builder.Build();
            var executeResult = _executeContext.Execute(ExecuteType.INSERT, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
            model.Id = (Int32)executeResult.Value;
            return model;
        }

        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            _statementStore.AddWhere(where);

            BuilderBase<TModel> builder = new ModifyBuilder<TModel>(model, _statementStore, true);
            var translationResult = builder.Build();
            var executeResult = _executeContext.Execute(ExecuteType.UPDATE, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
            return (Int32)executeResult.Value > 0;
        }

        public IList<TModel> Find<TModel>(Expression<Func<TModel, Boolean>> where) where TModel : PropertyMonitor, new()
        {
            return Find(where, null, null, null);
        }

        public IList<TModel> Find<TModel>(Expression<Func<TModel, Boolean>> where, Expression<Func<TModel, dynamic>> fields) where TModel : PropertyMonitor, new()
        {
            return Find(where, fields, null, null);
        }

        public IList<TModel> Find<TModel>(Expression<Func<TModel, dynamic>> fields) where TModel : PropertyMonitor, new()
        {
            return Find(null, fields, null, null);
        }

        public IList<TModel> Find<TModel>(Expression<Func<TModel, dynamic>> fields, Int32 pageIndex, Int32 pageSize) where TModel : PropertyMonitor, new()
        {
            return Find(null, fields, pageIndex, pageSize);
        }

        public IList<TModel> Find<TModel>(Expression<Func<TModel, Boolean>> where = null,
            Expression<Func<TModel, dynamic>> fields = null,
            Int32? pageIndex = null,
            Int32? pageSize = null) where TModel : PropertyMonitor, new()
        {
            _statementStore.AddWhere(where);

            BuilderBase<TModel> builder = new SelectBuilder<TModel>(_statementStore, fields, pageIndex, pageSize);
            var translationResult = builder.Build();
            var executeResult = _executeContext.Execute(ExecuteType.SELECT, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
            _statementStore.Clear();
            var dataTable = executeResult.Value as DataTable;
            return dataTable.AsList<TModel>();
        }

        public EntityMapper OrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : PropertyMonitor, new()
        {
            _statementStore.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public EntityMapper OrderByDesc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : PropertyMonitor, new()
        {
            _statementStore.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public Int32 Count<TModel>(Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            _statementStore.AddWhere(where);

            BuilderBase<TModel> builder = new SelectBuilder<TModel>(_statementStore, d => "COUNT(*)");
            var translationResult = builder.Build();
            var executeResult = _executeContext.Execute(ExecuteType.SELECTSINGLE, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
            return (Int32)executeResult.Value;
        }

        public EntityMapper LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.LEFT);
            return this;
        }

        public EntityMapper RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.RIGHT);
            return this;
        }

        public EntityMapper InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.INNER);
            return this;
        }

        public IList<TModel> ComplexSqlExecute<TModel>(String sql, IEnumerable<ParameterMapper> sqlParameters = null) where TModel : PropertyMonitor, new()
        {
            var executeResult = _executeContext.Execute(ExecuteType.SELECT, sql, sqlParameters, CommandType.Text);
            var dataTable = executeResult.Value as DataTable;
            return dataTable.AsList<TModel>();
        }

        public void Dispose()
        {
            _executeContext.Dispose();
        }
    }
}
