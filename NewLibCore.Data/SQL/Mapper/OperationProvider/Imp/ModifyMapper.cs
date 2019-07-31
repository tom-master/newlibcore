using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;

namespace NewLibCore.Data.SQL.Mapper.OperationProvider.Imp
{
    internal class ModifyMapper<TModel> : IModifyMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly ExpressionSegment _expressionSegment;

        public ModifyMapper()
        {
            _expressionSegment = new ExpressionSegment();
        }

        public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
        {
            _expressionSegment.Add(expression);

            Builder<TModel> builder = new ModifyBuilder<TModel>(model, _expressionSegment, true);
            var translateResult = builder.CreateTranslateResult();
            return (Int32)translateResult.Execute().Value > 0;
        }
    }
}