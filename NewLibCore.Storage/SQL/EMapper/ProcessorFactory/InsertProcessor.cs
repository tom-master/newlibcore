using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    /// <summary>
    /// 新增操作处理
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class InsertProcessor : Processor
    {
        /// <summary>
        /// 初始化一个InsertHandler类的实例
        /// </summary>
        /// <param name="model">要插入的模型</param>
        public InsertProcessor(TemplateBase templateBase, ConditionProcessor conditionProcessor, IOptions<EntityMapperOptions> options) : base(templateBase, conditionProcessor, options)
        {
        }

        protected override SqlExecuteResultConvert Execute(SqlComponent sqlComponent)
        {
            Check.IfNullOrZero(sqlComponent);

            var instance = sqlComponent.Model;
            instance.SetAddTime();
            instance.OnChanged();
            if (Options.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }
            var result = ConditionProcessor.Process(new ParseModel
            {
                Sql = TemplateBase.CreateInsert(instance),
                Parameters = instance.GetSqlElements().Parameters
            });
            return result.Execute();
        }
    }
}
