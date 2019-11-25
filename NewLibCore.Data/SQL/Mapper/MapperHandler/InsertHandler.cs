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
    internal class InsertHandler<TModel> : Handler where TModel : EntityBase, new()
    {
        private readonly TModel _instance;

        /// <summary>
        /// 初始化一个InsertHandler类的实例
        /// </summary>
        /// <param name="model">要插入的模型</param>
        internal InsertHandler(TModel model, IServiceProvider serviceProvider) : base(serviceProvider)
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

            var insert = String.Format(TemplateBase.InsertTemplate, typeof(TModel).GetTableName().TableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), TemplateBase.Extension.Identity);
            return ParserResult.CreateResult().Append(insert, propertyInfos.Select(c => new MapperParameter(c.Key, c.Value))).Execute(ServiceProvider);
        }
    }
}
