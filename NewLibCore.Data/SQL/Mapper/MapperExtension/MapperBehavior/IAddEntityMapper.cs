using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension.MapperBehavior
{
    public interface IAddEntityMapper<TModel> where TModel : EntityBase, new()
    {
        TModel Add(TModel model);
    }
}
