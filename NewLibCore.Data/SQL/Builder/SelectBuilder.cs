using System;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Builder
{
    internal class SelectBuilder<TModel> : IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly StatementStore _statementStore;

        internal SelectBuilder(StatementStore statementStore)
        {
            _statementStore = statementStore;
        }

        public TranslationCoreResult Build()
        {
            var translation = new TranslationCore();
            
            var fields = ExtractFieldsAndTableName(_statementStore.SelectFields);

            translation.TranslationResult.Append($@"SELECT {fields.fields} FROM {typeof(TModel).Name} AS {fields.tableAliasName} ");
            //_statementStore.AliasName = fields.tableAliasName;

            translation.Translate(_statementStore);

            if (_statementStore != null && _statementStore.ConditionExpression != null)
            {
                translation.TranslationResult.Append($@" AND {fields.tableAliasName}.IsDeleted = 0");
            }
            else
            {
                translation.TranslationResult.Append($@" WHERE {fields.tableAliasName}.IsDeleted = 0");
            }

            if (_statementStore.OrderByType != null)
            {
                var order = ExtractFieldsAndTableName(_statementStore.OrderExpression);
                var orderTemplate = MapperFactory.Instance.OrderByBuilder(_statementStore.OrderByType.Value, $@"{order.tableAliasName}.{order.fields}");
                translation.TranslationResult.Append(orderTemplate);
            }

            if (_pageIndex != null && _pageSize != null)
            {
                translation.TranslationResult
                    .Append(MapperFactory.Instance.Extension.Page.Replace("{value}", (_pageSize * (_pageIndex - 1)).ToString()).Replace("{pageSize}", _pageSize.ToString()));
            }

            return translation.TranslationResult;
        }

        private (String fields, String tableAliasName) ExtractFieldsAndTableName(Expression expression)
        {
            var fields = (LambdaExpression)expression;
            var modelAliasName = "";
            if (fields == null)
            {
                var modelType = typeof(TModel);
                modelAliasName = modelType.Name.ToLower();
                var propertys = modelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                return (String.Join(",", propertys.Select(s => $@"{modelAliasName}.{s.Name}")), modelAliasName);
            }

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
