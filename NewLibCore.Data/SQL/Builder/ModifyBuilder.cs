using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.DataStore;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Linq;

namespace NewLibCore.Data.SQL.Builder
{
    internal class ModifyBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isValidate;

        public ModifyBuilder(TModel model, Boolean isValidate = false) : base(model)
        {
            _isValidate = isValidate;
        }

        protected internal override TranslationResult Build(StatementStore statementStore = null)
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

            var translation = new TranslationToSql();
            translation.TranslationResult.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", properties.Select(s => $@"{s.Name}=@{s.Name}"))}");
            translation.TranslationResult.AppendParameter(properties.ToList().Select(c => new SqlParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray());
            if (statementStore != null && statementStore.Expression != null)
            {
                translation.Translate(statementStore);
            }
            translation.TranslationResult.Append($@"{SwitchDatabase.DatabaseSyntax.RowCountSuffix}");
            return translation.TranslationResult;
        }
    }
}
