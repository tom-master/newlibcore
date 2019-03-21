using NewLibCore.Data.SQL.InternalTranslation;
using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

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

        protected internal override void InternalBuildTail(TranslationToSql translation)
        {
            if (_statementStore != null && _statementStore.Expression != null)
            {
                translation.Translate(_statementStore);
            }
            translation.TranslationResult.Append($@"{DatabaseConfig.DatabaseSyntax.RowCountSuffix}");
        }

        protected internal override void InternalBuildHead(IList<PropertyInfo> properties, TranslationToSql translation)
        {
            translation.TranslationResult.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", properties.Select(s => $@"{s.Name}=@{s.Name}"))}");
            translation.TranslationResult.AppendParameter(properties.ToList().Select(c => new EntityParameter($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray());
        }

        protected internal override IList<PropertyInfo> ValidateModel()
        {
            var properties = ModelInstance.PropertyInfos;
            if (!properties.Any())
            {
                throw new ArgumentNullException("没有找到需要更新的字段");
            }
            if (_isValidate)
            {
                ModelInstance.Validate(properties);
            }

            return properties;
        }
    }
}
