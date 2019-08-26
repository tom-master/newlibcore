using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{

    /// <summary>
    /// 新增操作处理
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class InsertHandler<TModel> : Handler<TModel> where TModel : EntityBase, new()
    {
        private readonly Boolean _isVerifyModel;
        private readonly TModel _instance;

        /// <summary>
        /// 初始化一个InsertHandler类的实例
        /// </summary>
        /// <param name="model">要插入的模型</param>
        /// <param name="isVerifyModel">是否验证模型</param>
        internal InsertHandler(TModel model, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);
            _isVerifyModel = isVerifyModel;
            _instance = model;
        }

        protected override TranslationResult ExecuteTranslate()
        {
            _instance.OnChanged();
            if (_isVerifyModel)
            {
                _instance.Validate();
            }

            var propertyInfos = _instance.GetChangedProperty();
            var template = BuildTemplate();
            return TranslationResult.CreateTranslationResult().Append(template, propertyInfos.Select(c => new EntityParameter(c.Key, c.Value)));
        }

        private String BuildTemplate()
        {
            var propertyInfos = _instance.GetChangedProperty();
            var tableName = typeof(TModel).GetTableName().TableName;
            return String.Format(Instance.AddTemplate, tableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), Instance.Extension.Identity);
        }
    }
}
