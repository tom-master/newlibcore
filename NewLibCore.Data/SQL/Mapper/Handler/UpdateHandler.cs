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
    internal class UpdateHandler<TModel> : HandlerBase where TModel : EntityBase, new()
    {
        private readonly TModel _instance;

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
            _instance = model;
        }

        protected override ExecuteResult Execute()
        {
            _instance.SetUpdateTime();

            if (MapperConfig.EnableModelValidate)
            {
                _instance.Validate();
            }

            var expressionStore = new ExpressionStore();
            expressionStore.AddWhere(_filter);

            var propertys = _instance.GetChangedPropertys();
            if (!propertys.Any())
            {
                throw new Exception("没有获取到值发生变更的属性");
            }
            var (tableName, aliasName) = _instance.GetType().GetTableName();
            var updateFields = String.Join(",", propertys.Select(p => $@"{aliasName}.{p.Key}=@{p.Key}"));
            var parameters = propertys.Select(c => new MapperParameter(c.Key, c.Value));

            ResultExecutor.AppendResult(String.Format(Template.Update, tableName, aliasName, updateFields), parameters);
            
            var (whereSql, whereParameters) = Parser.Execute(expressionStore);
            ResultExecutor.AppendResult(whereSql, whereParameters);
            ResultExecutor.AppendResult($@"{PredicateType.AND} {aliasName}.{nameof(_instance.IsDeleted)}=0");
            ResultExecutor.AppendResult($@"{Template.RowCount}");
            _instance.Reset();

            return ResultExecutor.Execute();
        }
    }
}
