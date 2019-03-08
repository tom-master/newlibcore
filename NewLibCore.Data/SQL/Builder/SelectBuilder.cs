using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Linq.Expressions;

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

            if (_fields == null)
            {
                translation.TemporaryStore.Append($@"SELECT * FROM {typeof(TModel).Name} ");
            }

            translation.Translate(_where);
            return translation.TemporaryStore;
        }
    }
}
