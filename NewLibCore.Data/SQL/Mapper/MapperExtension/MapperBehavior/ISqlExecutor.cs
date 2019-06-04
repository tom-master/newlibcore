using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension.MapperBehavior
{
    public interface ISqlExecutor
    {
        List<TModel> ToList<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new();

        TModel ToSingle<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new();
    }
}
