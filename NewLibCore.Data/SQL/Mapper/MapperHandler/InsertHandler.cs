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

        /// <summary>
        /// 初始化一个InsertHandler类的实例
        /// </summary>
        /// <param name="model">要插入的模型</param>
        internal InsertHandler(TModel model)
        {
            Parameter.Validate(model);
            _instance = model;
        }

        /// <summary>
        /// 执行插入操作的翻译
        /// </summary>
        /// <returns></returns>
        internal override RawResult Execute()
        {
            _instance.OnChanged();
            if (MapperConfig.EnableModelValidate)
            {
                _instance.Validate();
            }

            var propertyInfos = _instance.GetChangedProperty();

            var tableName = typeof(TModel).GetTableName().TableName;
            var template = String.Format(MapperConfig.Instance.AddTemplate, tableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), MapperConfig.Instance.Extension.Identity);
            return TranslateResult.CreateResult().Append(template, propertyInfos.Select(c => new EntityParameter(c.Key, c.Value))).Execute();
        }
    }
}
