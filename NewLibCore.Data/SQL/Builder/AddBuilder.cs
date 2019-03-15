using NewLibCore.Data.SQL.DataStore;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal class AddBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;

        internal AddBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
        {
            _isVerifyModel = isVerifyModel;
        }

        protected internal override TranslationResult Build(StatementStore statementStore = null)
        {
            var translationResult = new TranslationResult();
            var propertyInfos = ModelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());

            if (!propertyInfos.Any())
            {
                throw new Exception($@"{ModelType.Name}:没有要插入的列");
            }

            if (_isVerifyModel)
            {
                ModelInstance.ValidateModel(propertyInfos);
            }

            translationResult.Append($@" INSERT {ModelType.Name} ({String.Join(",", propertyInfos.Select(c => c.Name))} ) VALUES ({String.Join(",", propertyInfos.Select(key => $@"@{key.Name}"))}) {SwitchDatabase.DatabaseSyntax.IdentitySuffix}");

            translationResult
                .AppendParameter(propertyInfos.ToList().Select(c => new SqlParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray());

            return translationResult;
        }
    }
}
