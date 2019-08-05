using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Builder
{

    /// <summary>
    /// 新增操作builder类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class AddBuilder<TModel> : Builder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;
        private readonly TModel _instance;

        internal AddBuilder(TModel model, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);
            _isVerifyModel = isVerifyModel;
            _instance = model;
        }

        /// <summary>
        /// 创建一个新增操作的翻译结果
        /// </summary>
        /// <returns></returns>
        protected override TranslationResult ExecuteSegmentTranslate()
        {
            _instance.OnChanged();
            if (_isVerifyModel)
            {
                _instance.Validate();
            }

            var propertyInfos = _instance.GetChangedProperty();
            var template = String.Format(MapperConfig.DatabaseConfig.AddTemplate, typeof(TModel).GetTableName().TableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), MapperConfig.DatabaseConfig.Extension.Identity);
            var translationResult = new TranslationResult();
            translationResult.Append(template, propertyInfos.Select(c => new EntityParameter(c.Key, c.Value)));
            _instance.Reset();
            return translationResult;
        }
    }
}
