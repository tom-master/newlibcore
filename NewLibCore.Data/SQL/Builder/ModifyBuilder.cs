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

            var buildEntry = new TranslationToSql();
            buildEntry.TemporaryStore.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", properties.Select(s => $@"{s.Name}=@{s.Name}"))}");
            buildEntry.TemporaryStore.AppendParameter(properties.ToList().Select(c => new SqlParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray());
            if (statementStore != null && statementStore.Expression != null)
            {
                buildEntry.Translate(statementStore);
            }
            buildEntry.TemporaryStore.Append(" ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c");
            return buildEntry.TemporaryStore;
        }
    }
}
