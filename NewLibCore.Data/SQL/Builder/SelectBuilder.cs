using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.InternalDataStore;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal class SelectBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Expression<Func<TModel, dynamic>> _fields;

        internal SelectBuilder(Expression<Func<TModel, dynamic>> fields = null) : base(null)
        {
            _fields = fields;
        }

        protected internal override SqlTemporaryStore Build(StatementStore statementStore = null)
        {
            var translation = new TranslationToSql();

            var fields = ExtractFieldsAndTableName(_fields);
            translation.TemporaryStore.Append($@"SELECT {fields.fields} FROM {typeof(TModel).Name} AS {fields.tableAliasName} ");
            statementStore.AliasName = fields.tableAliasName;
            translation.Translate(statementStore);
            if (statementStore != null && statementStore.Expression != null)
            {
                translation.TemporaryStore.Append($@" AND {fields.tableAliasName}.IsDeleted = 0");
            }
            else
            {
                translation.TemporaryStore.Append($@" WHERE {fields.tableAliasName}.IsDeleted = 0");
            }
            Console.WriteLine(translation.TemporaryStore.SqlStore.ToString());
            return translation.TemporaryStore;
        }

        private (String fields, String tableAliasName) ExtractFieldsAndTableName(Expression<Func<TModel, dynamic>> fields)
        {
            var modelAliasName = "";
            if (fields == null)
            {
                var modelType = typeof(TModel);
                modelAliasName = modelType.Name.ToLower();
                var propertys = modelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                return (String.Join(",", propertys.Select(s => $@"{modelAliasName}.{s.Name}")), modelAliasName);
            }
            modelAliasName = fields.Parameters[0].Type.Name.ToLower();
            var dynamicFields = (fields.Body as NewExpression).Members.Select(s => $@"{modelAliasName}.{s.Name}");
            return (String.Join(",", dynamicFields), modelAliasName);
        }
    }
}
