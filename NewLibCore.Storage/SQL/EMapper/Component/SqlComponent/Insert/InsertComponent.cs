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
        private readonly EntityMapperOptions _options;
        private EntityBase _model;

        public InsertComponent(ProcessExecutor processExecutor, IOptions<EntityMapperOptions> options)
        {
            Check.IfNullOrZero(processExecutor);
            Check.IfNullOrZero(options);

            _processExecutor = processExecutor;
            _options = options.Value;
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
                 if (_options.EnableModelValidate)
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
