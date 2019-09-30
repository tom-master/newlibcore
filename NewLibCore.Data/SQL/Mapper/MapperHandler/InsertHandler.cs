using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Database;
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
        internal InsertHandler(TModel model, IMapperDbContext mapperDbContext) : base(mapperDbContext)
        {
            Parameter.Validate(model);
            _instance = model;
        }

        internal override RawResult Execute()
        {
            _instance.OnChanged();
            if (MapperConfig.EnableModelValidate)
            {
                _instance.Validate();
            }

            var propertyInfos = _instance.GetChangedProperty();

            var tableName = typeof(TModel).GetTableName().TableName;
            var template = ReplacePlaceholder(propertyInfos, tableName);
            return TranslateResult.CreateResult().Append(template, propertyInfos.Select(c => new EntityParameter(c.Key, c.Value))).Execute(MapperDbContext);
        }
 

        private String ReplacePlaceholder(IReadOnlyList<KeyValuePair<String, Object>> propertyInfos, String tableName)
        {
            return String.Format(Instance.AddTemplate, tableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), Instance.Extension.Identity);
        }
    }
}
