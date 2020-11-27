using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component
{
    public class UpdateComponent : PredicateExpressionTranslator, IEntityMapperExecutor
    {
        internal EntityBase Model { get; set; }

        internal WhereComponent WhereComponent { get; private set; }

        internal FromComponent FromComponent { get; private set; }

        private readonly EntityMapperOptions _options;

        private readonly ResultExecutor _resultExecutor;

        public string ComponentIdentity => this.GetType().Name;

        public UpdateComponent(IOptions<EntityMapperOptions> options, ResultExecutor resultExecutor) : base(options)
        {
            _options = options.Value;
            _resultExecutor = resultExecutor;
        }

        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            Model = model;
        }

        internal void AddWhereComponent(WhereComponent whereComponent)
        {
            Check.IfNullOrZero(whereComponent);
            WhereComponent = whereComponent;
        }

        internal void AddFromComponent(FromComponent fromComponent)
        {
            Check.IfNullOrZero(fromComponent);
            FromComponent = fromComponent;
        }

        public ExecutorResult Execute()
        {
            return RunDiagnosis.Watch(() =>
            {
                var instance = Model;
                instance.SetUpdateTime();

                if (_options.EnableModelValidate)
                {
                    instance.CheckPropertyValue();
                }

                var (_, aliasName) = instance.GetEntityBaseAliasName();
                var update = _options.TemplateBase.CreateUpdate(instance);
                var statementResultBuilder = Translate(update, WhereComponent);
                instance.Reset();

                return _resultExecutor.Execute(statementResultBuilder);
            });
        }
    }
}