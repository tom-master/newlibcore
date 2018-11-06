using NewLibCore.Data.Mapper.BuildExtension;
using NewLibCore.Data.Mapper.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.Mapper
{
    internal class SelectBuilder<TModel> : SqlBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private Expression<Func<TModel, Boolean>> _join;

        internal SelectBuilder(TModel model, Expression<Func<TModel, Boolean>> join, JoinType joinType = JoinType.INNER) : base(model)
        {
            _join = join;
        }


        protected internal override BuildEntry<TModel> Build()
        {
            return null;
        }
    }

    internal enum JoinType
    {
        INNER = 1,
        LEFT = 2,
        RIGHT = 3
    }
}
