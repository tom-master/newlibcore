using System;
using NewLibCore.Data.SQL.Mapper.Builder;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper.MapperHandler.Imp
{
    internal class AddHandler<TModel> : IAddHandler<TModel> where TModel : EntityBase, new()
    {
        public TModel Add(TModel model)
        {
            Builder<TModel> builder = new AddBuilder<TModel>(model, true);
            var executeResult = builder.GetSegmentResult().GetExecuteResult();
            Int32.TryParse(executeResult.Value.ToString(), out var modelId);
            model.Id = modelId;
            return model;
        }
    }
}