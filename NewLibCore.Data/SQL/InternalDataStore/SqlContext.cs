using NewLibCore.Data.SQL.Builder;
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
        public InternalSqlContext Context { get; private set; }

        public SqlContext()
        {
            Context = new InternalSqlContext();
        }

        public TModel Add<TModel>(TModel model) where TModel : DomainModelBase, new()
        {
            BuilderBase<TModel> builder = new AddBuilder<TModel>(model, true);
            var entry = builder.Build();
            var returnValue = Context.Execute(ExecuteType.INSERT, entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            model.Id = (Int32)returnValue.MarshalValue;
            return model;
        }

        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new ModifyBuilder<TModel>(model, where, true);
            var entry = builder.Build();
            var returnValue = Context.Execute(ExecuteType.UPDATE, entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            return (Int32)returnValue.MarshalValue > 0;
        }

        public IList<TModel> Find<TModel>(Expression<Func<TModel, Boolean>> where, Expression<Func<TModel, dynamic>> fields = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new SelectBuilder<TModel>(where, fields);
            var entry = builder.Build();
            var returnValue = Context.Execute(ExecuteType.SELECT, entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            var dataTable = returnValue.MarshalValue as DataTable;
            return dataTable.AsList<TModel>();
        }

        public void Dispose()
        {
            Context.Dispose();
        }
    }
}
