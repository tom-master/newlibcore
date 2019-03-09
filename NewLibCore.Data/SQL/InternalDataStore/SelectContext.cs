using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.DataExtension;
using NewLibCore.Data.SQL.PropertyExtension;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class SelectContext : SqlContext
    {
        public SelectContext(string connection) : base(connection)
        {

        }

        public IList<TModel> Select<TModel>(Expression<Func<TModel, Boolean>> where, Expression<Func<TModel, dynamic>> fields) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new SelectBuilder<TModel>(where, fields);
            var entry = builder.Build();
            var returnValue = Execute(entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            var dataTable = returnValue.MarshalValue as DataTable;
            return dataTable.AsList<TModel>();
        }

        protected override void InternalExecute(DbCommand dbCommand, TemporaryMarshalValue temporaryMarshalValue)
        {
            var dr = dbCommand.ExecuteReader();
            var tmpDt = new DataTable("tmpDt");
            tmpDt.Load(dr, LoadOption.Upsert);
            dr.Close();
            temporaryMarshalValue.MarshalValue = tmpDt;
        }
    }
}