using NewLibCore.Data.SQL.DataMapper;
using NewLibCore.Data.SQL.InternalExecute;
using NewLibCore.Data.SQL.InternalTranslation;
using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Linq;

namespace NewLibCore.Data.SQL.Builder
{
    internal class ModifyBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isValidate;
        private readonly StatementStore _statementStore;

        public ModifyBuilder(TModel model, StatementStore statementStore, Boolean isValidate = false) : base(model)
        {
            _isValidate = isValidate;
            _statementStore = statementStore;
        }

        protected internal override TranslationResult Build()
        {
            var properties = ModelInstance.PropertyInfos;
            if (!properties.Any())
            {
                throw new ArgumentNullException("没有找到需要更新的字段");
            }
            ModelInstance.SetUpdateTime();
            if (_isValidate)
            {
                ModelInstance.Validate(properties);
            }

            var translation = new TranslationCore();
            translation.TranslationResult.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", properties.Select(s => $@"{s.Name}=@{s.Name}"))}", properties.Select(c => new EntityParameter($@"@{c.Name}", c.GetValue(ModelInstance))));

            if (_statementStore != null && _statementStore.Expression != null)
            {
                translation.Translate(_statementStore);
            }
            translation.TranslationResult.Append($@"{MapperFactory.Instance.GetStatementExtension().RowCount}");
            return translation.TranslationResult;
        }
    }
}
