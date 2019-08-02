using System;
using NewLibCore.Data.SQL.Mapper.Builder;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper.OperationProvider.Imp
{
    internal class AddMapper<TModel> : IAddMapper<TModel> where TModel : EntityBase, new()
    {
        public AddMapper()
        {
        }

        public TModel Add(TModel model)
        {
            Builder<TModel> builder = new AddBuilder<TModel>(model, true);
            var executeResult = builder.CreateResult().Execute();
            Int32.TryParse(executeResult.Value.ToString(), out var modelId);
            model.Id = modelId;
            return model;
        }
    }
}