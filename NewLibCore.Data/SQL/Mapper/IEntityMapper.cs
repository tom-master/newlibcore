using System;
using System.Collections.Generic;
using System.Data;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Mapper
{
    internal interface IAddEntityMapper<TModel> where TModel : EntityBase, new()
    {
        TModel Add(TModel model);
    }

    internal interface IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
    {
        Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression);
    }

    internal interface ISelectEntityMapper<TModel> where TModel : EntityBase, new()
    {

    }

    public class UpdateEntityMapper<TModel> : IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly StatementStore _statementStore;
        private readonly ExecuteCore _executeCore;

        public UpdateEntityMapper()
        {
            _statementStore = new StatementStore();
            _executeCore = new ExecuteCore();
        }

        public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
        {
            _statementStore.AddWhere(expression);

            IBuilder<TModel> builder = new ModifyBuilder<TModel>(model, _statementStore, true);
            var translationResult = builder.Build();
            var executeResult = _executeCore.Execute(ExecuteType.UPDATE, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
            return (Int32)executeResult.Value > 0;
        }
    }

    public class AddEntityMapper<TModel> : IAddEntityMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly ExecuteCore _executeCore;

        public AddEntityMapper()
        {
            _executeCore = new ExecuteCore();
        }

        public TModel Add(TModel model)
        {
            IBuilder<TModel> builder = new AddBuilder<TModel>(model, true);
            var translationResult = builder.Build();
            var executeResult = _executeCore.Execute(ExecuteType.INSERT, translationResult.SqlStore.ToString(), translationResult.ParameterStore, CommandType.Text);
            model.Id = (Int32)executeResult.Value;
            return model;
        }
    }
}
