using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.DataExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.DataStore
{
    public class Repository : IDisposable
    {
        private readonly StatementStore _statementStore;

        public Repository()
        {
            Context = new InternalSqlContext();
            _statementStore = new StatementStore();
        }

        public InternalSqlContext Context { get; private set; }

        public TModel Add<TModel>(TModel model) where TModel : DomainModelBase, new()
        {
            BuilderBase<TModel> builder = new AddBuilder<TModel>(model, true);
            var store = builder.Build();
            var returnValue = Context.Execute(ExecuteType.INSERT, store.SqlStore.ToString(), store.ParameterStore, CommandType.Text);
            model.Id = (Int32)returnValue.MarshalValue;
            return model;
        }

        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new ModifyBuilder<TModel>(model, true);
            _statementStore.AddWhere(where);
            var store = builder.Build(_statementStore);
            var returnValue = Context.Execute(ExecuteType.UPDATE, store.SqlStore.ToString(), store.ParameterStore, CommandType.Text);
            return (Int32)returnValue.MarshalValue > 0;
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
            BuilderBase<TModel> builder = new SelectBuilder<TModel>(fields, pageIndex, pageSize);
            _statementStore.AddWhere(where);
            var store = builder.Build(_statementStore);
            var returnValue = Context.Execute(ExecuteType.SELECT, store.SqlStore.ToString(), store.ParameterStore, CommandType.Text);
            var dataTable = returnValue.MarshalValue as DataTable;
            return dataTable.AsList<TModel>();
        }

        public Repository OrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : PropertyMonitor, new()
        {
            _statementStore.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public Repository OrderByDesc<TModel, TKey>(Expression<Func<TModel, TKey>> order) where TModel : PropertyMonitor, new()
        {
            _statementStore.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public Int32 Count<TModel>(Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new SelectBuilder<TModel>(d => "COUNT(*)");
            _statementStore.AddWhere(where);
            var store = builder.Build(_statementStore);
            var returnValue = Context.Execute(ExecuteType.SELECTSINGLE, store.SqlStore.ToString(), store.ParameterStore, CommandType.Text);
            return (Int32)returnValue.MarshalValue;
        }

        public Repository LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.LEFT);
            return this;
        }

        public Repository RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.RIGHT);
            return this;
        }

        public Repository InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.INNER);
            return this;
        }

        public void Dispose()
        {
            Context.Dispose();
        }
    }
}
