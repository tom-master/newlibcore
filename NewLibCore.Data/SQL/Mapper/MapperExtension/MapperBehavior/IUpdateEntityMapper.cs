using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension.MapperBehavior
{
    public interface IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
    {
        Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression);
    }
}
