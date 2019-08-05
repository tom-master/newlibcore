using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Builder;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;

namespace NewLibCore.Data.SQL.Mapper.MapperStandardHandler.Imp
{
    internal class ModifyMapper<TModel> : IModifyMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly SegmentManager _segmentManager = new SegmentManager();

        public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
        {
            _segmentManager.Add(expression);

            Builder<TModel> builder = new ModifyBuilder<TModel>(model, _segmentManager, true);
            var translateResult = builder.GetSegmentResult();
            return (Int32)translateResult.GetExecuteResult().Value > 0;
        }
    }
}