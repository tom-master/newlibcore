using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension.MapperBehavior
{
    /// <summary>
    /// 查询操作
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    public interface ISelectEntityMapper<TModel> where TModel : EntityBase, new()
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
        ISelectEntityMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null);

        /// <summary>
        /// 构建Select表达式
        /// </summary>
        /// <param name="fields"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> Select<T>(Expression<Func<TModel, T, dynamic>> fields = null) where T : EntityBase, new();

        /// <summary>
        /// 构建Where条件
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression = null) where T : EntityBase, new();

        /// <summary>
        /// 构建Where条件
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression = null) where T : EntityBase, new();

        /// <summary>
        /// 构建Where条件
        /// </summary>
        /// <param name="expression"></param>
        /// <returns></returns>
        ISelectEntityMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression = null);

        /// <summary>
        /// 构建分页对象
        /// </summary>
        /// <param name="pageIndex"></param>
        /// <param name="pageSize"></param>
        /// <returns></returns>
        ISelectEntityMapper<TModel> Page(Int32 pageIndex, Int32 pageSize);

        /// <summary>
        /// 构建左连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        /// <summary>
        /// 构建右连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        /// <summary>
        /// 构建内连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        /// <summary>
        /// 构建左连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TLeft"></typeparam>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        /// <summary>
        /// 构建右连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TLeft"></typeparam>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        /// <summary>
        /// 构建内连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TLeft"></typeparam>
        /// <typeparam name="TRight"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        /// <summary>
        /// 构建排序对象
        /// </summary>
        /// <param name="order"></param>
        /// <param name="orderBy"></param>
        /// <typeparam name="TOrder"></typeparam>
        /// <typeparam name="TKey"></typeparam>
        /// <returns></returns>
        ISelectEntityMapper<TModel> OrderBy<TOrder, TKey>(Expression<Func<TOrder, TKey>> order, OrderByType orderBy = OrderByType.DESC) where TOrder : EntityBase, new();
    }
}
