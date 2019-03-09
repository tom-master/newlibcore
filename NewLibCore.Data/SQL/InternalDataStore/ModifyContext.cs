using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Data;
using System.Data.Common;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class ModifyContext : SqlContext
    {
        public ModifyContext(String connection) : base(connection)
        {
        }

        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new ModifyBuilder<TModel>(model, where, true);
            var entry = builder.Build();
            var returnValue = Execute(entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            return (Int32)returnValue.MarshalValue > 0;
        }

        protected override void InternalExecute(DbCommand dbCommand, TemporaryMarshalValue temporaryMarshalValue)
        {
            var count = Int32.Parse(dbCommand.ExecuteNonQuery().ToString());
            temporaryMarshalValue.MarshalValue = count;
        }
    }
}
