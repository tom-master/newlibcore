using System;
using System.Linq;
using Microsoft.Extensions.DependencyInjection;
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

            var databaseConfig = ServiceProvider.GetService<InstanceConfig>();

            var tableName = typeof(TModel).GetTableName().TableName;
            var template = String.Format(databaseConfig.AddTemplate, tableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), databaseConfig.Extension.Identity);
            return ParserResult.CreateResult().Append(template, propertyInfos.Select(c => new EntityParameter(c.Key, c.Value))).Execute(ServiceProvider);
        }
    }
}
