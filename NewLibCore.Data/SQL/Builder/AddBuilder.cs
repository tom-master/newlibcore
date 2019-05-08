using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Builder
{
    internal class AddBuilder<TModel> : IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;
        private readonly IEnumerable<PropertyInfo> _propertyInfos;
        private readonly TModel _instance;

        internal AddBuilder(TModel model, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);

            _isVerifyModel = isVerifyModel;
            _propertyInfos = typeof(TModel).GetProperties();
            _instance = model;
        }

        public TranslationCoreResult Build()
        {
            var propertyInfos = _propertyInfos.Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
            if (!propertyInfos.Any())
            {
                throw new Exception($@"{typeof(TModel).Name}:没有要插入的列");
            }

            _instance.SetAddTime();
            if (_isVerifyModel)
            {
                _instance.Validate(propertyInfos);
            }
            var translationResult = new TranslationCoreResult();
            var fields = String.Join(",", propertyInfos.Select(c => c.Name));
            var placeHolder = String.Join(",", propertyInfos.Select(key => $@"@{key.Name}"));
            var entityParameters = propertyInfos.Select(c => new EntityParameter($@"@{c.Name}", c.GetValue(_instance)));

            translationResult.Append($@" INSERT {typeof(TModel).Name} ({fields}) VALUES ({placeHolder}) {MapperFactory.Mapper.Extension.Identity}", entityParameters);
            return translationResult;
        }
    }
}
