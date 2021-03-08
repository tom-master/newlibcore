using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class InsertComponent<TModel> where TModel : EntityBase, new()
    {
        private readonly TemplateBase _templateBase;
        private readonly ProcessExecutor _processExecutor;
        private readonly EntityMapperOptions _entityMapperOptions;

        internal InsertComponent(TemplateBase templateBase, ProcessExecutor processExecutor, IOptions<EntityMapperOptions> options)
        {
            Check.IfNullOrZero(templateBase);
            Check.IfNullOrZero(processExecutor);
            Check.IfNullOrZero(options);

            _templateBase = templateBase;
            _processExecutor = processExecutor;
            _entityMapperOptions = options.Value;
        }

        protected SqlExecuteResultConvert Execute(TModel model)
        {
            Check.IfNullOrZero(model);
            var instance = model;
            instance.SetAddTime();
            instance.OnChanged();
            if (_entityMapperOptions.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }
            var insert = _templateBase.CreateInsert(instance);
            _processExecutor.Append(insert, instance.GetSqlElements().Parameters);
            return _processExecutor.Execute();
        }
    }
}
