using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper.Component.SqlComponent;
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
        private readonly PredicateExpressionTranslatorResultExecutor _processResultExecutor;

        private EntityBase _model;

        public string ComponentIdentity => this.GetType().Name;

        public InsertComponent(IOptions<EntityMapperOptions> options, PredicateExpressionTranslatorResultExecutor predicateExpressionTranslatorResultExecutor)
        {
            Check.IfNullOrZero(options);

            _processResultExecutor = predicateExpressionTranslatorResultExecutor;
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
                 PredicateExpressionTranslatorResultBuilder predicateExpressionTranslatorResultBuilder = new PredicateExpressionTranslatorResultBuilder();
                 predicateExpressionTranslatorResultBuilder.StatmentTemplate = insert;
                 instance.GetSqlElements().Parameters.Select(s => predicateExpressionTranslatorResultBuilder.Parameters.Append(s));

                 return _processResultExecutor.Execute(predicateExpressionTranslatorResultBuilder);
             });
        }
    }
}
