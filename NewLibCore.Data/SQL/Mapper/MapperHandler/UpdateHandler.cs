using System;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
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

        private readonly IServiceProvider _serviceProvider;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="model">要更新的模型</param>
        public UpdateHandler(TModel model, Expression<Func<TModel, Boolean>> filter, IServiceProvider serviceProvider)
        {
            Parameter.Validate(model);
            Parameter.Validate(filter);

            _modelInstance = model;
            _filter = filter;
            _serviceProvider = serviceProvider;
        }

        internal override RawResult Execute()
        {
            _modelInstance.SetUpdateTime();

            if (MapperConfig.EnableModelValidate)
            {
                _modelInstance.Validate();
            }

            var (TableName, AliasName) = typeof(TModel).GetTableName();
            var propertys = _modelInstance.GetChangedProperty();

            var translateResult = TranslateResult.CreateResult();
            var parser = ExpressionParser.CreateParser(_serviceProvider);
            var databaseConfig = _serviceProvider.GetService<InstanceConfig>();

            var expressionStore = new ExpressionStore();
            expressionStore.AddWhere(_filter);

            var (sql, parameters) = parser.Parse(expressionStore);
            translateResult.Append(String.Format(databaseConfig.UpdateTemplate, TableName, AliasName, String.Join(",", propertys.Select(p => $@"{AliasName}.{p.Key}=@{p.Key}"))),propertys.Select(c => new EntityParameter(c.Key, c.Value)));
            translateResult.Append(sql,parameters);
            translateResult.Append($@"{RelationType.AND} {AliasName}.IsDeleted=0");
            translateResult.Append($@"{databaseConfig.Extension.RowCount}");
            _modelInstance.Reset();

            return translateResult.Execute(_serviceProvider);
        }
    }
}
