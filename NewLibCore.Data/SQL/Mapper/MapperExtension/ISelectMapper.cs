using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public interface ISelectMapper<TModel> where TModel : new()
    {
        Boolean Exist();

        Int32 Count();

        TModel FirstOrDefault();

        T FirstOrDefault<T>() where T : new();

        List<TModel> ToList();

        List<T> ToList<T>() where T : new();

        SelectMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null);

        SelectMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression);

        SelectMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new();

        SelectMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new();

        SelectMapper<TModel> Page(Int32 pageIndex, Int32 pageSize);

        SelectMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new();

        SelectMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new();

        SelectMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new();

        SelectMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
        where TLeft : new()
        where TRight : new();


        SelectMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
          where TLeft : new()
          where TRight : new();

        SelectMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
          where TLeft : new()
          where TRight : new();

        SelectMapper<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new();

        SelectMapper<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : new();

    }
}
