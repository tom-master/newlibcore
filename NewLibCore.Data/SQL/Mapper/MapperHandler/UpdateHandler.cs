using System;
using System.Collections.Generic;
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
        private readonly StatementStore _statementStore;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="model">要更新的模型</param>
        public UpdateHandler(TModel model, StatementStore statementStore)
        {
            Parameter.Validate(model);
            Parameter.Validate(statementStore);

            _modelInstance = model;
            _statementStore = statementStore;
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

            var translateContext = TranslateContext.CreateContext(_statementStore);
            translateContext.Result.Append(ReplacePlaceholder(TableName, AliasName, propertys), propertys.Select(c => new EntityParameter(c.Key, c.Value)));
            translateContext.Translate();
            translateContext.Result.Append($@"{RelationType.AND} {AliasName}.IsDeleted=0");
            _modelInstance.Reset();

            return translateContext.Result.Append($@"{Instance.Extension.RowCount}").Execute();
        }


        private String ReplacePlaceholder(String TableName, String AliasName, IReadOnlyList<KeyValuePair<String, Object>> propertys)
        {
            return String.Format(Instance.UpdateTemplate, TableName, AliasName, String.Join(",", propertys.Select(p => $@"{AliasName}.{p.Key}=@{p.Key}")));
        }
    }
}
