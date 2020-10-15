using System.Collections.Generic;
using System.Linq;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    public class UpdateComponent : PredicateProcessor
    {
        internal EntityBase Model { get; set; }

        internal WhereComponent WhereComponent { get; private set; }

        internal FromComponent FromComponent { get; private set; }

        private readonly EntityMapperOptions _options;

        private readonly PredicateProcessorResultExecutor _processResultExecutor;

        public UpdateComponent(IOptions<EntityMapperOptions> options) : base(options)
        {
            _options = options.Value;
            _processResultExecutor = new PredicateProcessorResultExecutor(options.Value.DbContext);
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

        internal ExecutorResult Execute()
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
                var predicateProcessResult = Process(WhereComponent, FromComponent);
                predicateProcessResult.StatmentTemplate = update;
                //predicateProcessResult.Sql.Append($@"{update} {PredicateType.AND} {aliasName}.{nameof(instance.IsDeleted)} = 0 {_options.TemplateBase.AffectedRows}");
                instance.Reset();

                return _processResultExecutor.Execute(predicateProcessResult);
            });
        }
    }
}