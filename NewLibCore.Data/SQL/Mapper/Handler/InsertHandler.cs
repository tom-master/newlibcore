using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 新增操作处理
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class InsertHandler
    {
        private readonly TemplateBase _templateBase;

        private readonly ParserExecutor _parserExecutor;

        /// <summary>
        /// 初始化一个InsertHandler类的实例
        /// </summary>
        /// <param name="model">要插入的模型</param>
        public InsertHandler(TemplateBase templateBase, ParserExecutor parserExecutor)
        {
            Parameter.Validate(templateBase);
            Parameter.Validate(parserExecutor);

            _templateBase = templateBase;
            _parserExecutor = parserExecutor;
        }

        /// <summary>
        /// 执行插入操作的翻译
        /// </summary>
        /// <returns></returns>
        internal ExecuteResult Execute<TModel>(TModel instance) where TModel : EntityBase, new()
        {
            instance.OnChanged();
            if (EntityMapper.EnableModelValidate)
            {
                instance.Validate();
            }

            var propertys = instance.GetChangedPropertys();
            if (!propertys.Any())
            {
                throw new Exception("没有获取到值发生变更的属性");
            }
            var insertFields = String.Join(",", propertys.Select(c => c.Key));
            var placeHolders = String.Join(",", propertys.Select(key => $@"@{key.Key}"));
            var (tableName, _) = instance.GetType().GetTableName();

            var insert = _templateBase.CreateInsert(tableName, insertFields, placeHolders);
            var parameters = propertys.Select(c => new MapperParameter(c.Key, c.Value));
            var result = _parserExecutor.Parse(new ParseModel
            {
                Sql = insert,
                Parameters = parameters
            });
            return result.Execute();
        }
    }
}
