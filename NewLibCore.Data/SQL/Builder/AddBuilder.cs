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
        private readonly TModel _instance;

        internal AddBuilder(TModel model, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);

            _isVerifyModel = isVerifyModel;
            _instance = model;
        }

        public TranslationCoreResult Build()
        {
            _instance.OnChanged();
            if (_isVerifyModel)
            {
                _instance.Validate();
            }

            var propertyInfos = _instance.GetPropertys();
            var translationResult = new TranslationCoreResult();
            var fields = String.Join(",", propertyInfos.Select(c => c.Key));
            var placeHolder = String.Join(",", propertyInfos.Select(key => $@"@{key.Key}"));
            var entityParameters = propertyInfos.Select(c => new EntityParameter($@"@{c.Key}", c.Value));

            translationResult.Append($@" INSERT {typeof(TModel).GetAliasName()} ({fields}) VALUES ({placeHolder}) {MapperFactory.Mapper.Extension.Identity}", entityParameters);
            return translationResult;
        }
    }
}
