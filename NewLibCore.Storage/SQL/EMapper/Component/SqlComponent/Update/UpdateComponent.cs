using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class UpdateComponent<TModel> where TModel : EntityBase, new()
    {
        internal EntityBase Model { get; set; }

        internal WhereComponent WhereComponent { get; private set; }

        internal FromComponent FromComponent { get; private set; }

        private readonly TemplateBase _templateBase;
        private readonly ConditionProcessor _conditionProcessor;
        private readonly EntityMapperOptions _entityMapperOptions;

        public UpdateComponent(TemplateBase templateBase, ConditionProcessor conditionProcessor, IOptions<EntityMapperOptions> options)
        {
            _templateBase = templateBase;
            _conditionProcessor = conditionProcessor;
            _entityMapperOptions = options.Value;
        }

        internal void AddWhereComponent(TModel model, WhereComponent whereComponent)
        {
            Check.IfNullOrZero(model);
            Check.IfNullOrZero(whereComponent);

            Model = model;
            WhereComponent = whereComponent;
        }

        internal void AddFromComponent(FromComponent fromComponent)
        {
            Check.IfNullOrZero(fromComponent);
            FromComponent = fromComponent;
        }

        protected SqlExecuteResultConvert Execute()
        {
            var instance = Model;
            instance.SetUpdateTime();

            if (_entityMapperOptions.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }

            var (_, aliasName) = instance.GetEntityBaseAliasName();
            var update = _templateBase.CreateUpdate(instance);
            var result = _conditionProcessor.Process(null, WhereComponent);

            result.Append($@"{update} {PredicateType.AND} {aliasName}.{nameof(instance.IsDeleted)} = 0 {_templateBase.AffectedRows}");
            instance.Reset();

            return result.Execute();
        }
    }
}