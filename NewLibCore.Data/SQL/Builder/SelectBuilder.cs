using NewLibCore.Data.SQL.InternalExecute;
using NewLibCore.Data.SQL.InternalTranslation;
using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.MapperExtension.PropertyExtension;
using System;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal class SelectBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Expression<Func<TModel, dynamic>> _fields;
        private readonly Int32? _pageIndex;
        private readonly Int32? _pageSize;

        private readonly StatementStore _statementStore;

        internal SelectBuilder(StatementStore statementStore, Expression<Func<TModel, dynamic>> fields = null, Int32? pageIndex = null, Int32? pageSize = null) : base(null)
        {
            _fields = fields;
            _statementStore = statementStore;

            _pageIndex = pageIndex;
            _pageSize = pageSize;
        }

        protected internal override TranslationResult Build()
        {
            var translation = new TranslationToSql();
            var fields = ExtractFieldsAndTableName(_fields);

            translation.TranslationResult.Append($@"SELECT {fields.fields} FROM {typeof(TModel).Name} AS {fields.tableAliasName} ");
            _statementStore.AliasName = fields.tableAliasName;
            translation.Translate(_statementStore);

            if (_statementStore != null && _statementStore.Expression != null)
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
                translation.TranslationResult.Append($@" {String.Format(_statementStore.OrderByType.GetDescription(), $@"{order.tableAliasName}.{order.fields}")}");
            }
            if (_pageIndex != null && _pageSize != null)
            {
                translation.TranslationResult.Append(DatabaseConfig.DatabaseSyntax.Page.Replace("{value}", (_pageSize * (_pageIndex - 1)).ToString()).Replace("{pageSize}", _pageSize.ToString()));
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
