using System;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 更新处理类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class UpdateHandler<TModel> : HandlerBase where TModel : PropertyMonitor, new()
    {
        private readonly TModel _instance;

        private readonly ExpressionStore _expressionStore;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="model">要更新的模型</param>
        public UpdateHandler(TModel model, ExpressionStore expressionStore, IServiceProvider serviceProvider) : base(serviceProvider)
        {
            Parameter.Validate(model);
            Parameter.Validate(expressionStore);

            _expressionStore = expressionStore;
            _instance = model;
        }

        protected override ExecuteResult Execute()
        {
            _instance.SetUpdateTime();

            if (MapperConfig.EnableModelValidate)
            {
                _instance.Validate();
            }

            var propertys = _instance.GetChangedPropertys();
            if (!propertys.Any())
            {
                throw new Exception("没有获取到值发生变更的属性");
            }
            var (tableName, aliasName) = _instance.GetType().GetTableName();
            var updateFields = String.Join(",", propertys.Select(p => $@"{aliasName}.{p.Key}=@{p.Key}"));
            var parameters = propertys.Select(c => new MapperParameter(c.Key, c.Value));
            var update = Template.CreateUpdate(tableName, aliasName, updateFields);

            var result = ParserExecutor.Parse(new ParseModel
            {
                Sql = update,
                Parameters = parameters,
                ExpressionStore = _expressionStore
            });

            result.Append($@"{PredicateType.AND} {aliasName}.IsDeleted=0");
            result.Append($@"{Template.RowCount}");
            _instance.Reset();

            return result.Execute();
        }
    }
}
