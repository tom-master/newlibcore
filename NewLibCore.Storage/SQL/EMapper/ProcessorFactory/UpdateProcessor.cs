using System;
using System.Linq;
using NewLibCore.Data.SQL.EMapper;
using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Template;

namespace NewLibCore.Data.SQL.ProcessorFactory
{
    /// <summary>
    /// 更新处理类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class UpdateProcessor : Processor
    {

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="templateBase"></param>
        /// <param name="expressionProcessor"></param>
        /// <returns></returns>
        public UpdateProcessor(TemplateBase templateBase, ExpressionProcessor expressionProcessor) : base(templateBase, expressionProcessor)
        {
        }

        protected override SqlExecuteResultConvert Execute(ExpressionStore store)
        {
            var instance = store.Model;
            instance.SetUpdateTime();

            if (EntityMapperConfig.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }

            var (_, aliasName) = instance.GetEntityBaseAliasName();
            var result = _expressionProcessor.Processor(new ParseModel
            {
                Sql = _templateBase.CreateUpdate(instance),
                Parameters = instance.SqlPart.Parameters,
                ExpressionStore = store
            });

            result.Append($@"{PredicateType.AND} {aliasName}.{nameof(instance.IsDeleted)}=0 {_templateBase.AffectedRows}");
            instance.Reset();

            return result.Execute();
        }
    }
}
