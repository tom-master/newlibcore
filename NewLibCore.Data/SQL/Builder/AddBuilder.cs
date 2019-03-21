using NewLibCore.Data.SQL.InternalTranslation;
using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.MapperExtension.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal class AddBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;
        private readonly IEnumerable<PropertyInfo> _propertyInfos;

        internal AddBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
        {
            _isVerifyModel = isVerifyModel;
            _propertyInfos = ModelType.GetProperties();
        }

        protected internal override IList<PropertyInfo> ValidateModel()
        {
            var propertyInfos = _propertyInfos.Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
            if (!propertyInfos.Any())
            {
                throw new Exception($@"{ModelType.Name}:没有要插入的列");
            }

            ModelInstance.SetAddTime();
            if (_isVerifyModel)
            {
                ModelInstance.Validate(propertyInfos);
            }
            return propertyInfos.ToList();
        }

        protected internal override void InternalBuildHead(IList<PropertyInfo> properties, TranslationToSql translation)
        {
            var fields = String.Join(",", properties.Select(c => c.Name));
            var parameterPrefix = String.Join(",", properties.Select(key => $@"@{key.Name}"));
            translation.TranslationResult.Append($@" INSERT {ModelType.Name} ({fields} ) VALUES ({parameterPrefix}) {DatabaseConfig.DatabaseSyntax.IdentitySuffix}");

            var parameters = properties.ToList().Select(c => new EntityParameter($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray();
            translation.TranslationResult.AppendParameter(parameters);
        }
    }
}
