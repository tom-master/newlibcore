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
        private readonly StatementStore _statementStore;

        public ModifyMapper()
        {
            _executionCore = new ExecutionCore();
            _statementStore = new StatementStore();
        }

        public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
        {
            _statementStore.Add(expression);
            _statementStore.ExecuteType = ExecuteType.UPDATE;
            IBuilder<TModel> builder = new ModifyBuilder<TModel>(model, _statementStore, true);
            var executeResult = _executionCore.Execute(ExecuteType.UPDATE, builder.CreateTranslateResult());
            return (Int32)executeResult.Value > 0;
        }
    }
}