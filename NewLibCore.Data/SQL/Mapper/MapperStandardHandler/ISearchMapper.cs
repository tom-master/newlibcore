using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

namespace NewLibCore.Data.SQL.Mapper.MapperStandardHandler
{
    /// <summary>
    /// 查询操作
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    public interface ISearchMapper<TModel> where TModel : EntityBase, new()
    {
        /// <summary>
        /// 返回一个TModel对象
        /// </summary>
        /// <returns></returns>
        TModel FirstOrDefault();

        /// <summary>
        /// 返回一个TModel的对象列表
        /// </summary>
        /// <returns></returns>
        List<TModel> ToList();

        /// <summary>
        /// 获取数量
        /// </summary>
        /// <returns></returns>
        Int32 Count();

        /// <summary>
        /// 判断是否存在
        /// </summary>
        /// <returns></returns>
        Boolean Exist();

        /// <summary>
        /// 构建Select表达式
        /// </summary>
        /// <param name="fields"></param>
        /// <returns></returns>
        ISearchMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null);

        /// <summary>
        /// 构建Select表达式
        /// </summary>
        /// <param name="fields"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> Select<T>(Expression<Func<TModel, T, dynamic>> fields = null) where T : EntityBase, new();

        /// <summary>
        /// 构建Where条件
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : EntityBase, new();

        /// <summary>
        /// 构建Where条件
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : EntityBase, new();

        /// <summary>
        /// 构建Where条件
        /// </summary>
        /// <param name="expression"></param>
        /// <returns></returns>
        ISearchMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression);

        /// <summary>
        /// 构建分页对象
        /// </summary>
        /// <param name="pageIndex"></param>
        /// <param name="pageSize"></param>
        /// <returns></returns>
        ISearchMapper<TModel> Page(Int32 pageIndex, Int32 pageSize);

        /// <summary>
        /// 构建左连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        /// <summary>
        /// 构建右连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        /// <summary>
        /// 构建内连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        /// <summary>
        /// 构建左连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TLeft"></typeparam>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        /// <summary>
        /// 构建右连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TLeft"></typeparam>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        /// <summary>
        /// 构建内连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TLeft"></typeparam>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISearchMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        /// <summary>
        /// 构建降序排序对象
        /// </summary>
        /// <typeparam name="TOrder"></typeparam>
        /// <typeparam name="TKey"></typeparam>
        /// <param name="order"></param>
        /// <returns></returns>
        ISearchMapper<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : EntityBase, new();

        /// <summary>
        /// 构建升序排序对象
        /// </summary>
        /// <typeparam name="TOrder"></typeparam>
        /// <typeparam name="TKey"></typeparam>
        /// <param name="order"></param>
        /// <returns></returns>
        ISearchMapper<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : EntityBase, new();
    }
}
