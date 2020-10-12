using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;
using System;
namespace NewLibCore.Storage.SQL.Component.Sql
{
    public class InsertComponent
    {
        private readonly TemplateBase _templateBase;
        private readonly EntityMapperOptions _options;
        private readonly PredicateProcessorResultExecutor _processResultExecutor;

        private EntityBase _model;

        public InsertComponent(MapperDbContextBase mapperDbContextBase, IOptions<EntityMapperOptions> options)
        {
            Check.IfNullOrZero(mapperDbContextBase);
            Check.IfNullOrZero(options);

            _processResultExecutor = new PredicateProcessorResultExecutor(mapperDbContextBase);
            _options = options.Value;
        }

        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            _model = model;
        }

        internal ExecutorResult Execute()
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
                 return _processResultExecutor.Execute(insert, instance.GetSqlElements().Parameters);
             });
        }
    }
}
