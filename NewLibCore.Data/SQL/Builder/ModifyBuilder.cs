using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.InternalDataStore;
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

        protected internal override SqlTemporaryStore Build(StatementStore statementStore = null)
        {
            var properties = ModelInstance.PropertyInfos;
            if (!properties.Any())
            {
                throw new ArgumentNullException("没有找到需要更新的字段");
            }
            ModelInstance.SetUpdateTime();
            if (_isValidate)
            {
                ValidateModel(properties);
            }

            var translation = new TranslationToSql();
            translation.TemporaryStore.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", properties.Select(s => $@"{s.Name}=@{s.Name}"))}");
            translation.TemporaryStore.AppendParameter(properties.ToList().Select(c => new SqlParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray());
            if (statementStore != null && statementStore.Expression != null)
            {
                translation.Translate(statementStore);
            }
            translation.TemporaryStore.Append($@"{SwitchDatabase.RowCountSuffix}");
            return translation.TemporaryStore;
        }
    }
}
