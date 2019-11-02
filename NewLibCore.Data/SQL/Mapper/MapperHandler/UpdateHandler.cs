using System;
using System.Linq;
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

        private readonly ExpressionStore _expressionStore;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="model">要更新的模型</param>
        public UpdateHandler(TModel model, ExpressionStore expressionStore)
        {
            Parameter.Validate(model);
            Parameter.Validate(expressionStore);

            _modelInstance = model;
            _expressionStore = expressionStore;
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
            var parser = ExpressionParser.CreateParser();

            translateResult.Append(String.Format(MapperConfig.Instance.UpdateTemplate, TableName, AliasName, String.Join(",", propertys.Select(p => $@"{AliasName}.{p.Key}=@{p.Key}"))));
            translateResult.Append(parser.Parse(_expressionStore).ToString());
            translateResult.Append($@"{RelationType.AND} {AliasName}.IsDeleted=0");
            translateResult.Append($@"{MapperConfig.Instance.Extension.RowCount}");
            _modelInstance.Reset();

            return translateResult.Execute();
        }
    }
}
