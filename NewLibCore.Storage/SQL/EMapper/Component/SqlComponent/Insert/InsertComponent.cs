using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper.Extension;
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

                 if (_options.ModelValidate)
                 {
                     instance.CheckPropertyValue();
                 }
                 var (tableName, aliasName) = instance.GetEntityBaseAliasName();

                 var sqlElements = instance.GetChangedProperties().GetSqlElements();
                 var insert = _templateBase.CreateInsert(tableName, sqlElements.Fields, sqlElements.InsertPlaceHolders);

                 var statementResultBuilder = new StatementResultBuilder();
                 statementResultBuilder.AddStatementTemplate(insert);

                 foreach (var item in sqlElements.Parameters)
                 {
                     statementResultBuilder.AddParameter(item);
                 }
                 return _resultExecutor.Execute(statementResultBuilder);
             });
        }
    }
}
