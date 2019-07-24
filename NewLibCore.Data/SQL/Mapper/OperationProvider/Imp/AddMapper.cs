using System;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Execute;

namespace NewLibCore.Data.SQL.Mapper.OperationProvider.Imp
{
    internal class AddMapper<TModel> : IAddMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly ExecutionCore _executionCore;

        public AddMapper()
        {
            _executionCore = new ExecutionCore();
        }

        public TModel Add(TModel model)
        {
            IBuilder<TModel> builder = new AddBuilder<TModel>(model, true);
            var executeResult = _executionCore.Execute(ExecuteType.INSERT, builder.CreateTranslateResult());
            Int32.TryParse(executeResult.Value.ToString(), out var modelId);
            model.Id = modelId;
            return model;
        }

        public void Commit()
        {
            _executionCore.Commit();
        }

        public void OpenTransaction()
        {
            _executionCore.OpenTransaction();
        }

        public void Rollback()
        {
            _executionCore.Rollback();
        }
    }
}