using NewLibCore.Data.SQL.BuildExtension;
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
        private readonly Expression<Func<TModel, Boolean>> _where;
        private readonly Expression<Func<TModel, dynamic>> _fields;


        internal SelectBuilder(Expression<Func<TModel, Boolean>> where = null, Expression<Func<TModel, dynamic>> fields = null) : base(null)
        {
            _where = where;
            _fields = fields;
        }

        protected internal override SqlTemporaryStore Build()
        {
            var translation = new TranslationToSql();

            var fields = ExtractFieldsAndTableName(_fields);
            translation.TemporaryStore.Append($@"SELECT {fields.fields} FROM {typeof(TModel).Name} {fields.tableAliasName} ");
            if (_where != null)
            {
                translation.Translate(_where);
                translation.TemporaryStore.Append($@" AND {fields.tableAliasName}IsDeleted = 0");
            }
            else
            {
                translation.TemporaryStore.Append($@" WHERE {fields.tableAliasName}IsDeleted = 0");
            }

            return translation.TemporaryStore;
        }

        private (String fields, String tableAliasName) ExtractFieldsAndTableName(Expression<Func<TModel, dynamic>> fields)
        {
            if (fields == null)
            {
                var propertys = typeof(TModel).GetProperties().Where(w => w.GetCustomAttributes().Any(a => a.GetType() == typeof(PropertyKeyAttribute)));
                return (String.Join(",", propertys.Select(s => s.Name)), "");
            }

            var modelAliasName = fields.Parameters[0].Name;
            var dynamicFields = (fields.Body as NewExpression).Members.Select(s => $@"{modelAliasName}.{s.Name}");
            return (String.Join(",", dynamicFields), $@"AS {(String.IsNullOrEmpty(modelAliasName) ? "" : modelAliasName + ".")}");
        }
    }
}
