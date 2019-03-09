using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Data;
using System.Data.Common;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class AddContext : SqlContext
    {
        public AddContext(String connection) : base(connection)
        {
        }

        public TModel Add<TModel>(TModel model) where TModel : DomainModelBase, new()
        {
            try
            {
                BuilderBase<TModel> builder = new AddBuilder<TModel>(model, true);
                var entry = builder.Build();
                var returnValue = Execute(entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
                model.Id = (Int32)returnValue.MarshalValue;
                return model;
            }
            catch (Exception)
            {
                throw;
            }
        }

        protected override void InternalExecute(DbCommand dbCommand, TemporaryMarshalValue temporaryMarshalValue)
        {
            var id = Int32.Parse(dbCommand.ExecuteScalar().ToString());
            temporaryMarshalValue.MarshalValue = id;
        }
    }
}
