using NewLibCore.Data.SQL.Mapper.EntityExtension; 

namespace NewLibCore.Data.SQL.Mapper.OperationProvider
{
    /// <summary>
    /// 新增操作
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    public interface IAddMapper<TModel> where TModel : EntityBase, new()
    {
        /// <summary>
        /// 新增一个TModel
        /// </summary>
        /// <param name="model"></param>
        /// <returns></returns>
        TModel Add(TModel model);
    }
}
