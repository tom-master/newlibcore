using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.DataMapper;
using NewLibCore.Data.SQL.InternalExecute;
using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.MapperExtension.PropertyExtension;

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

        protected internal override TranslationResult Build()
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
            var fields = String.Join(",", propertyInfos.Select(c => c.Name));
            var parameterPrefix = String.Join(",", propertyInfos.Select(key => $@"@{key.Name}"));

            var translationResult = new TranslationResult();
            translationResult.Append($@" INSERT {ModelType.Name} ({fields} ) VALUES ({parameterPrefix}) {DatabaseConfig.DatabaseSyntax.IdentitySuffix}"
                , propertyInfos.Select(c => new EntityParameter($@"@{c.Name}", c.GetValue(ModelInstance))));

            return translationResult;
        }
    }
}
