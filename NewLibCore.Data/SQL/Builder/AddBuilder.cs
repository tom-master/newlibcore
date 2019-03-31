using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Data.SQL.Mapper.Translation;

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

        protected internal override TranslationCoreResult Build()
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
            var translationResult = new TranslationCoreResult();
            var fields = String.Join(",", propertyInfos.Select(c => c.Name));
            var placeHolder = String.Join(",", propertyInfos.Select(key => $@"@{key.Name}"));
            var entityParameters = propertyInfos.Select(c => new EntityParameter($@"@{c.Name}", c.GetValue(ModelInstance)));

            translationResult.Append($@" INSERT {ModelType.Name} ({fields}) VALUES ({placeHolder}) {MapperFactory.Instance.Extension.Identity}", entityParameters);
            return translationResult;
        }
    }
}
