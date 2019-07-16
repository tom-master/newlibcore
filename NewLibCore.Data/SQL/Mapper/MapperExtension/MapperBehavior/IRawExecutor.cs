using System;
using System.Collections.Generic;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension.MapperBehavior
{
    public interface IRawExecutor
    {
        /// <summary>
        /// 获取一个TModel的对象列表
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        List<TModel> ToList<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new();

        /// <summary>
        /// 获取单个TModel对象
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        TModel ToSingle<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new();
    }
}
