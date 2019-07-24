using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper.OperationProvider
{
    /// <summary>
    /// 更新操作
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal interface IModifyMapper<TModel> : ITransactionController where TModel : EntityBase, new()
    {
        /// <summary>
        /// 更新一个TModel
        /// </summary>
        /// <param name="model"></param>
        /// <param name="expression"></param>
        /// <returns></returns>
        Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression);
    }
}
