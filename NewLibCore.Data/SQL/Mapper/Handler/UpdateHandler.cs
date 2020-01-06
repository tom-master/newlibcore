using System;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 更新处理类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class UpdateHandler
    { 
        private readonly TemplateBase _templateBase;

        private readonly ParserExecutor _parserExecutor;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="templateBase"></param>
        /// <param name="parserExecutor"></param>
        public UpdateHandler(TemplateBase templateBase, ParserExecutor parserExecutor)
        {
            Parameter.Validate(templateBase);
            Parameter.Validate(parserExecutor);

            _templateBase = templateBase;
            _parserExecutor = parserExecutor;
        }

        internal ExecuteResult Execute<TModel>(TModel instance, ExpressionStore expressionStore) where TModel : EntityBase, new()
        {
            instance.SetUpdateTime();

            if (EntityMapper.EnableModelValidate)
            {
                instance.Validate();
            }

            var propertys = instance.GetChangedPropertys();
            if (!propertys.Any())
            {
                throw new Exception("没有获取到值发生变更的属性");
            }
            var (tableName, aliasName) = instance.GetType().GetTableName();
            var updateFields = String.Join(",", propertys.Select(p => $@"{aliasName}.{p.Key}=@{p.Key}"));
            var parameters = propertys.Select(c => new MapperParameter(c.Key, c.Value));
            var update = _templateBase.CreateUpdate(tableName, aliasName, updateFields);

            var result = _parserExecutor.Parse(new ParseModel
            {
                Sql = update,
                Parameters = parameters,
                ExpressionStore = expressionStore
            });

            result.Append($@"{PredicateType.AND} {aliasName}.IsDeleted=0");
            result.Append($@"{_templateBase.RowCount}");
            instance.Reset();

            return result.Execute();
        }
    }
}
