using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{

    /// <summary>
    /// 新增操作处理
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class InsertHandler<TModel> : Handler where TModel : EntityBase, new()
    {
        private readonly TModel _instance;
        private readonly Boolean _verifyModel;

        /// <summary>
        /// 初始化一个InsertHandler类的实例
        /// </summary>
        /// <param name="model">要插入的模型</param>
        /// <param name="isVerifyModel">是否验证模型</param>
        internal InsertHandler(TModel model, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);
            _verifyModel = isVerifyModel;
            _instance = model;
        }

        protected override TranslationResult ExecuteTranslate()
        {
            _instance.OnChanged();
            if (_verifyModel)
            {
                _instance.Validate();
            }

            var propertyInfos = _instance.GetChangedProperty();

            var tableName = typeof(TModel).GetTableName().TableName;
            var template = ReplacePlaceholder(propertyInfos, tableName);
            return TranslationResult.CreateTranslationResult().Append(template, CreateParameter(propertyInfos));
        }

        private static IEnumerable<EntityParameter> CreateParameter(IReadOnlyList<KeyValuePair<String, Object>> propertyInfos)
        {
            return propertyInfos.Select(c => new EntityParameter(c.Key, c.Value));
        }

        private String ReplacePlaceholder(IReadOnlyList<KeyValuePair<String, Object>> propertyInfos, String tableName)
        {
            return String.Format(Instance.AddTemplate, tableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), Instance.Extension.Identity);
        }
    }
}
