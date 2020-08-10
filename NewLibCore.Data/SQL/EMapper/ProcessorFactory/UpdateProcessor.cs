using System;
using System.Linq;
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
        /// <param name="parserExecutor"></param>
        public UpdateProcessor(TemplateBase templateBase, ParserExecutor parserExecutor) : base(templateBase, parserExecutor)
        {
        }

        protected override ExecuteResult Execute(ExpressionStore store)
        {
            var instance = store.Model;
            instance.SetUpdateTime();

            if (EntityMapper.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }

            var (_, aliasName) = instance.GetTableName();
            var result = _parserExecutor.Parse(new ParseModel
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
