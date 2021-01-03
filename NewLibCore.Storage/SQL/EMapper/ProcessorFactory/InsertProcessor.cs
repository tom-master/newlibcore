using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Store;
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
        public InsertProcessor(TemplateBase templateBase, ExpressionProcessor expressionProcessor) : base(templateBase, expressionProcessor)
        {
        }

        protected override SqlExecuteResultConvert Execute(ExpressionStore store)
        {
            Check.IfNullOrZero(store);

            var instance = store.Model;
            instance.OnChanged();
            if (EntityMapperConfig.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }
            var result = _expressionProcessor.Processor(new ParseModel
            {
                Sql = _templateBase.CreateInsert(instance),
                Parameters = instance.GetSqlElements().Parameters
            });
            return result.Execute();
        }
    }
}
