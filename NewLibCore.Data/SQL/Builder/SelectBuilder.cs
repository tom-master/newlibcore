using System;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Builder
{
    internal class SelectBuilder<TModel> : IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly StatementStore _statementStore;

        internal SelectBuilder(StatementStore statementStore)
        {
            Parameter.Validate(statementStore);
            _statementStore = statementStore;
        }

        public TranslationCoreResult Build()
        {
            var translation = new TranslationCore(_statementStore);
          
            var fields = ExtractFieldsAndTableName(_statementStore.Field);
            translation.Result.Append($@"SELECT {fields.fields} FROM {typeof(TModel).Name} AS {fields.tableAliasName}");
            translation.Translate();

            var aliasMapper = _statementStore.MergeAliasMapper().Select(s => s.Value).Distinct();
            foreach (var aliasItem in aliasMapper)
            {
                translation.Result.Append($@"AND {aliasItem.ToLower()}.IsDeleted = 0");
            }

            if (_statementStore.Order != null)
            {
                var order = ExtractFieldsAndTableName(_statementStore.Order);
                var orderTemplate = MapperFactory.Mapper.OrderByBuilder(_statementStore.Order.OrderBy, $@"{order.tableAliasName}.{order.fields}");
                translation.Result.Append(orderTemplate);
            }

            if (_statementStore.Page != null)
            {
                var pageIndex = (_statementStore.Page.Size * (_statementStore.Page.Index - 1)).ToString();
                var pageSize = _statementStore.Page.Size.ToString();
                translation.Result.Append(MapperFactory.Mapper.Extension.Page.Replace("{value}", pageIndex).Replace("{pageSize}", pageSize));
            }

            return translation.Result;
        }

        private (String fields, String tableAliasName) ExtractFieldsAndTableName(Statement statement)
        {
            var modelAliasName = "";
            if (statement == null)
            {
                var modelType = typeof(TModel);
                modelAliasName = modelType.Name.ToLower();
                var propertys = modelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                return (String.Join(",", propertys.Select(s => $@"{modelAliasName}.{s.Name}")), modelAliasName);
            }

            var fields = (LambdaExpression)statement.Expression;
            modelAliasName = fields.Parameters[0].Type.Name.ToLower();
            if (fields.Body.NodeType == ExpressionType.Constant)
            {
                var constant = (ConstantExpression)fields.Body;
                return (constant.Value + "", modelAliasName);
            }
            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, modelAliasName);
            }

            var dynamicFields = (fields.Body as NewExpression).Members.Select(s => $@"{modelAliasName}.{s.Name}");
            return (String.Join(",", dynamicFields), modelAliasName);
        }
    }
}
