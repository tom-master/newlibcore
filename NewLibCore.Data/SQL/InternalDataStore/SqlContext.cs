using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.DataExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class SqlContext : IDisposable
    {
        private readonly StatementStore _statementStore;

        public SqlContext()
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

        public IList<TModel> Find<TModel>(Expression<Func<TModel, Boolean>> where = null, Expression<Func<TModel, dynamic>> fields = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new SelectBuilder<TModel>(fields);
            _statementStore.AddWhere(where);
            var store = builder.Build(_statementStore);
            var returnValue = Context.Execute(ExecuteType.SELECT, store.SqlStore.ToString(), store.ParameterStore, CommandType.Text);
            var dataTable = returnValue.MarshalValue as DataTable;
            return dataTable.AsList<TModel>();
        }

        public Int32 Count<TModel>(Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new SelectBuilder<TModel>(d => "COUNT(*)");
            _statementStore.AddWhere(where);
            var store = builder.Build(_statementStore);
            var returnValue = Context.Execute(ExecuteType.SELECTSINGLE, store.SqlStore.ToString(), store.ParameterStore, CommandType.Text);
            return (Int32)returnValue.MarshalValue;
        }

        public SqlContext LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.LEFT);
            return this;
        }

        public SqlContext RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
            where TRight : PropertyMonitor, new()
        {
            _statementStore.AddJoin(expression, JoinType.RIGHT);
            return this;
        }
        public SqlContext InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : PropertyMonitor, new()
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
