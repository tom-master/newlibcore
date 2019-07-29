using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Execute;

namespace NewLibCore.Data.SQL.Mapper.OperationProvider.Imp
{
    internal class ModifyMapper<TModel> : IModifyMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly ExecutionCore _executionCore;
        private readonly ExpressionSegment _expressionSegment;

        public ModifyMapper()
        {
            _executionCore = new ExecutionCore();
            _expressionSegment = new ExpressionSegment();
        }

        public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
        {
            _expressionSegment.Add(expression);
            _expressionSegment.ExecuteType = ExecuteType.UPDATE;

            IBuilder<TModel> builder = new ModifyBuilder<TModel>(model, _expressionSegment, true);
            var executeResult = _executionCore.Execute(builder.CreateTranslateResult());
            return (Int32)executeResult.Value > 0;
        }
    }
}