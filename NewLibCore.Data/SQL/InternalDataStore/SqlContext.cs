using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public sealed class SqlContext
    {
        public Boolean Add<TModel>(TModel model) where TModel : PropertyMonitor, new()
        {
            return false;
        }

        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression = null) where TModel : PropertyMonitor, new()
        {
            return false;
        }

        public TModel FindOne<TModel>(Expression<Func<TModel, Boolean>> expression = null) where TModel : PropertyMonitor, new()
        {
            return null;
        }

        public SqlContext InnerJoin<TMaster, TSalve>(Expression<Func<TMaster, TSalve>> expression) where TMaster : PropertyMonitor, new()
            where TSalve : PropertyMonitor, new()
        {
            return this;
        }

        public SqlContext LeftJoin<TMaster, TSalve>(Expression<Func<TMaster, TSalve>> expression) where TMaster : PropertyMonitor, new()
            where TSalve : PropertyMonitor, new()
        {
            return this;
        }

        public SqlContext RightJoin<TMaster, TSalve>(Expression<Func<TMaster, TSalve>> expression) where TMaster : PropertyMonitor, new()
            where TSalve : PropertyMonitor, new()
        {
            return this;
        }
    }
}
