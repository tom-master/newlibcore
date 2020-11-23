using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;
using System.Linq;

namespace NewLibCore.Storage.SQL.Component
{
    public class InsertComponent : IEntityMapperExecutor
    {
        private readonly TemplateBase _templateBase;
        private readonly EntityMapperOptions _options;
        private readonly ResultExecutor _resultExecutor;

        private EntityBase _model;

        public string ComponentIdentity => this.GetType().Name;

        public InsertComponent(IOptions<EntityMapperOptions> options, ResultExecutor resultExecutor)
        {
            Check.IfNullOrZero(options);

            _resultExecutor = resultExecutor;
            _options = options.Value;
        }

        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            _model = model;
        }

        public ExecutorResult Execute()
        {
            return RunDiagnosis.Watch(() =>
             {
                 Check.IfNullOrZero(_model);
                 var instance = _model;
                 instance.SetAddTime();
                 instance.OnChanged();
                 if (_options.EnableModelValidate)
                 {
                     instance.CheckPropertyValue();
                 }
                 var insert = _templateBase.CreateInsert(instance);
                 var statementResultBuilder = new StatementResultBuilder();
                 statementResultBuilder.StatmentTemplate = insert;
                 instance.GetSqlElements().Parameters.Select(s => statementResultBuilder.Parameters.Append(s));

                 return _resultExecutor.Execute(statementResultBuilder);
             });
        }
    }
}
