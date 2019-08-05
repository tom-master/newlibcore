using System;
using NewLibCore.Data.SQL.Mapper.Builder;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper.Mapper.Imp
{
    internal class AddMapper<TModel> : IAddMapper<TModel> where TModel : EntityBase, new()
    {
        public TModel Add(TModel model)
        {
            Handler<TModel> builder = new InsertHandler<TModel>(model, true);
            var executeResult = builder.GetSegmentResult().GetExecuteResult();
            Int32.TryParse(executeResult.Value.ToString(), out var modelId);
            model.Id = modelId;
            return model;
        }
    }
}