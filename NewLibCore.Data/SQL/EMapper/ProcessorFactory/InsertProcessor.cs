using NewLibCore.Data.SQL.EMapper;
using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.ProcessorFactory
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
            Parameter.IfNullOrZero(store);

            var instance = store.Model;
            instance.OnChanged();
            if (EntityMapperConfig.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }
            var result = _expressionProcessor.Processor(new ParseModel
            {
                Sql = _templateBase.CreateInsert(instance),
                Parameters = instance.SqlPart.Parameters
            });
            return result.Execute();
        }
    }
}
