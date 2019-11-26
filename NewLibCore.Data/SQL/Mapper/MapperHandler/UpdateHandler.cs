using System;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 更新处理类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class UpdateHandler<TModel> : Handler where TModel : EntityBase, new()
    {
        private readonly TModel _modelInstance;

        private readonly Expression<Func<TModel, Boolean>> _filter;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="model">要更新的模型</param>
        public UpdateHandler(TModel model, Expression<Func<TModel, Boolean>> filter, IServiceProvider serviceProvider) : base(serviceProvider)
        {
            Parameter.Validate(model);
            Parameter.Validate(filter);

            _filter = filter;
            _modelInstance = model;
        }

        internal override RawResult Execute()
        {
            _modelInstance.SetUpdateTime();

            if (MapperConfig.EnableModelValidate)
            {
                _modelInstance.Validate();
            }
            var expressionStore = new ExpressionStore();
            expressionStore.AddWhere(_filter);

            var (sql, parameters) = Parser.CreateParser(ServiceProvider).ExecuteParser(expressionStore);
            var parserResult = ParserResult.CreateResult();
            var propertys = _modelInstance.GetChangedProperty();
            var (TableName, AliasName) = typeof(TModel).GetTableName();

            parserResult.Append(String.Format(TemplateBase.UpdateTemplate, TableName, AliasName, String.Join(",", propertys.Select(p => $@"{AliasName}.{p.Key}=@{p.Key}"))), propertys.Select(c => new MapperParameter(c.Key, c.Value)));
            parserResult.Append(sql, parameters);
            parserResult.Append($@"{PredicateType.AND} {AliasName}.IsDeleted=0");
            parserResult.Append($@"{TemplateBase.RowCount}");
            _modelInstance.Reset();

            return parserResult.Execute(ServiceProvider);
        }
    }
}
