using System;
using System.Linq;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Store;
using NewLibCore.Storage.SQL.Template;

namespace NewLibCore.Storage.SQL.ProcessorFactory
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
        public UpdateProcessor(TemplateBase templateBase, ConditionProcessor conditionProcessor, EntityMapperOptions options) : base(templateBase, conditionProcessor, options)
        {
        }

        protected override SqlExecuteResultConvert Execute(ExpressionStore store)
        {
            var instance = store.Model;
            instance.SetUpdateTime();

            if (Options.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }

            var (_, aliasName) = instance.GetEntityBaseAliasName();
            var result = ConditionProcessor.Process(new ParseModel
            {
                Sql = TemplateBase.CreateUpdate(instance),
                Parameters = instance.GetSqlElements().Parameters,
                ExpressionStore = store
            });

            result.Append($@"{PredicateType.AND} {aliasName}.{nameof(instance.IsDeleted)} = 0 {TemplateBase.AffectedRows}");
            instance.Reset();

            return result.Execute();
        }
    }
}
