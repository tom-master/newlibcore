using System;
using System.Collections.Generic;
using System.Data;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper
{
    public interface IAddEntityMapper<TModel> where TModel : EntityBase, new()
    {
        TModel Add(TModel model);
    }

    public interface IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
    {
        Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression);
    }

    public interface ISelectEntityMapper<TModel> where TModel : EntityBase, new()
    {
        TModel FirstOrDefault();

        List<TModel> ToList();

        Int32 Count();

        Boolean Exist();

        ISelectEntityMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null);

        ISelectEntityMapper<TModel> Select<T>(Expression<Func<TModel, T, dynamic>> fields = null) where T : EntityBase, new();

        ISelectEntityMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression = null) where T : EntityBase, new();

        ISelectEntityMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression = null) where T : EntityBase, new();

        ISelectEntityMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression = null);

        ISelectEntityMapper<TModel> Page(Int32 pageIndex, Int32 pageSize);

        ISelectEntityMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        ISelectEntityMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        ISelectEntityMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new();

        ISelectEntityMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        ISelectEntityMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        ISelectEntityMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : EntityBase, new() where TRight : EntityBase, new();

        ISelectEntityMapper<TModel> OrderBy<TOrder, TKey>(Expression<Func<TOrder, TKey>> order, OrderByType orderBy = OrderByType.DESC) where TOrder : EntityBase, new();
    }
   
    public interface ISqlExecutor
    { 
        List<TModel> ToList<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new();

        TModel ToSingle<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new();
 
    } 
}
