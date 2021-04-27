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
        private readonly ProcessExecutor _processExecutor;
        private readonly EntityMapperOptions _entityMapperOptions;
        private EntityBase _model;

        public InsertComponent(TemplateBase templateBase, ProcessExecutor processExecutor, IOptions<EntityMapperOptions> options)
        {
            Check.IfNullOrZero(templateBase);
            Check.IfNullOrZero(processExecutor);
            Check.IfNullOrZero(options);

            _templateBase = templateBase;
            _processExecutor = processExecutor;
            _entityMapperOptions = options.Value;
        }

        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            _model = model;
        }

        internal SqlExecuteResultConvert Execute()
        {
            return RunDiagnosis.Watch(() =>
             {
                 Check.IfNullOrZero(_model);
                 var instance = _model;
                 instance.SetAddTime();
                 instance.OnChanged();
                 if (_entityMapperOptions.EnableModelValidate)
                 {
                     instance.CheckPropertyValue();
                 }
                 var insert = _templateBase.CreateInsert(instance);
                 _processExecutor.Append(insert, instance.GetSqlElements().Parameters);
                 return _processExecutor.Execute();
             });
        }
    }
}
