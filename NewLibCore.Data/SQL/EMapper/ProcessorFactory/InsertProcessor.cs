using System;
using System.Linq;
using NewLibCore.Data.SQL.Extension;
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
        public InsertProcessor(TemplateBase templateBase, ParserExecutor parserExecutor) : base(templateBase, parserExecutor)
        {
        }

        protected override ExecuteResult Execute(ExpressionStore store)
        {
            Parameter.Validate(store);

            var instance = store.Model;
            instance.OnChanged();
            if (EntityMapper.EnableModelValidate)
            {
                instance.CheckPropertyValue();
            }

            var propertys = instance.ChangedPropertys;
            if (!propertys.Any())
            {
                throw new Exception("没有获取到值发生变更的属性");
            }
            var insertFields = String.Join(",", propertys.Select(c => c.Key));
            var placeHolders = String.Join(",", propertys.Select(key => $@"@{key.Key}"));
            var (tableName, _) = instance.GetType().GetTableName();

            var parameters = propertys.Select(c => new MapperParameter(c.Key, c.Value));
            var result = _parserExecutor.Parse(new ParseModel
            {
                Sql = _templateBase.CreateInsert(tableName, insertFields, placeHolders),
                Parameters = parameters
            });
            return result.Execute();
        }
    }
}
