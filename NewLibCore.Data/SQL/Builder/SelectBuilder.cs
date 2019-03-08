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


        internal SelectBuilder(Expression<Func<TModel, Boolean>> where, Expression<Func<TModel, dynamic>> fields = null) : base(null)
        {
            _where = where;
            _fields = fields;
        }

        protected internal override SqlTemporaryStore Build()
        {
            var translation = new TranslationToSql();

            var fields = ExtractFields(_fields);
            translation.TemporaryStore.Append($@"SELECT {fields} FROM {typeof(TModel).Name} ");
            translation.Translate(_where);
            return translation.TemporaryStore;
        }

        private String ExtractFields(Expression<Func<TModel, dynamic>> fields)
        {
            if (fields == null)
            {
                var propertys = typeof(TModel).GetProperties().Where(w => !w.GetCustomAttributes().Any(a => a.GetType() == typeof(IgnoreAttribute)));
                return String.Join(",", propertys.Select(s => s.Name));
            }
            return "";
        }
    }
}
