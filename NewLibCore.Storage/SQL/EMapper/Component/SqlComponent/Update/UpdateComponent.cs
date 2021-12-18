using Microsoft.Extensions.Options;

using NewLibCore.Storage.SQL.EMapper.Extension;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;
namespace NewLibCore.Storage.SQL.Component
{
    public class UpdateComponent : QueryComponent
    {
        internal EntityBase Model { get; set; }

        private readonly ResultExecutor _resultExecutor;

        public UpdateComponent(IOptions<EntityMapperOptions> options, ResultExecutor resultExecutor) : base(options, resultExecutor)
        {
            _resultExecutor = resultExecutor;
        }

        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            Model = model;
        }

        public new ExecutorResult Execute()
        {
            return RunDiagnosis.Watch(() =>
            {
                var instance = Model;
                instance.SetUpdateTime();

                if (_options.ModelValidate)
                {
                    instance.CheckPropertyValue();
                }

                var (tableName, aliasName) = instance.GetEntityBaseAliasName();
                var sqlElements = instance.GetChangedProperties().GetSqlElements();
                var update = _options.TemplateBase.CreateUpdate(tableName, aliasName, sqlElements.UpdatePlaceHolders);
                var statementResultBuilder = Translate(this);
                statementResultBuilder.AddStatementTemplate(update);
                instance.Reset();

                return _resultExecutor.Execute(statementResultBuilder);
            });
        }
    }
}