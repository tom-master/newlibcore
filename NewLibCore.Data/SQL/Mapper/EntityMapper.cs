using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {
        private readonly ExecutionCore _executionCore;

        /// <summary>
        /// 初始化一个EntityMapper类的实例
        /// </summary>
        private EntityMapper()
        {
            _executionCore = MapperConfig.ServiceProvider.GetService<ExecutionCore>();
        }

        /// <summary>
        /// 初始化一个EntityMapper类的实例
        /// </summary>
        /// <returns></returns>
        public static EntityMapper CreateMapper()
        {
            return new EntityMapper();
        }

        /// <summary>
        /// 添加一個TModel
        /// </summary>
        /// <param name="model">要新增的对象</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);

            return RunDiagnosis.Watch(() =>
             {
                 Handler<TModel> builder = new InsertHandler<TModel>(model, true);
                 var translationResult = builder.GetTranslationResult();
                 model.Id = translationResult.ExecuteTranslateResult(_executionCore).ToPrimitive<Int32>();
                 return model;
             });
        }

        /// <summary>
        /// 修改一個TModel
        /// </summary>
        /// <param name="model">要修改的对象</param>
        /// <param name="expression">查询条件</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);
            Parameter.Validate(expression);

            return RunDiagnosis.Watch(() =>
             {
                 var segmentManager = new SegmentManager();
                 segmentManager.Add(expression);
                 Handler<TModel> builder = new UpdateHandler<TModel>(model, segmentManager, true);
                 return builder.GetTranslationResult().ExecuteTranslateResult(_executionCore).ToPrimitive<Int32>() > 0;
             });
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <param name="fields">字段</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public SelectMapper<TModel> Select<TModel>(Expression<Func<TModel, dynamic>> fields = null) where TModel : EntityBase, new()
        {
            return new SelectMapper<TModel>(_executionCore).Select(fields);
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <param name="fields">字段</param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public SelectMapper<TModel> Select<TModel, T>(Expression<Func<TModel, T, dynamic>> fields = null) where TModel : EntityBase, new()
        where T : EntityBase, new()
        {
            return new SelectMapper<TModel>(_executionCore).Select(fields);
        }

        /// <summary>
        /// 执行一個返回列表的sql语句
        /// </summary>
        /// <param name="sql">sql语句</param>
        /// <param name="parameters">实体参数</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public List<TModel> ExecuteToList<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            Parameter.Validate(sql);

            return RunDiagnosis.Watch(() =>
            {
                return RawExecute(sql, parameters).ToList<TModel>();
            });
        }

        /// <summary>
        /// 执行一个返回单个TModel的sql语句
        /// </summary>
        /// <param name="sql">sql语句</param>
        /// <param name="parameters">实体参数</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel ExecuteToSingle<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            Parameter.Validate(sql);

            return RunDiagnosis.Watch(() =>
            {
                var modelType = typeof(TModel);
                if (modelType.IsNumeric())
                {
                    return RawExecute(sql, parameters).ToPrimitive<TModel>();
                }

                return RawExecute(sql, parameters).ToSingle<TModel>();
            });
        }

        /// <summary>
        /// 直接执行sql语句
        /// </summary>
        /// <param name="sql">sql语句</param>
        /// <param name="parameters">实体参数</param>
        /// <returns></returns>
        private RawExecuteResult RawExecute(String sql, IEnumerable<EntityParameter> parameters = null)
        {
            var sqlResult = TranslationResult.CreateTranslationResult();
            sqlResult.Append(sql, parameters);
            return sqlResult.ExecuteTranslateResult(_executionCore);
        }

        public void OpenTransaction()
        {
            _executionCore.OpenTransaction();
        }

        public void Commit()
        {
            _executionCore.Commit();
        }

        public void Rollback()
        {
            _executionCore.Rollback();
        }

        public void Dispose()
        {
            _executionCore.Dispose();
        } 
    }

    public sealed class SelectMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly SegmentManager _segmentManager = new SegmentManager();

        private readonly ExecutionCore _executionCore;
        internal SelectMapper(ExecutionCore executionCore)
        {
            _executionCore = executionCore;
        }

        public Boolean Exist()
        {
            return Count() > 0;
        }

        public Int32 Count()
        {
            return RunDiagnosis.Watch(() =>
            {
                Select(s => "COUNT(*)");
                var executeResult = InternalExecuteSql();
                return executeResult.ToPrimitive<Int32>();
            });
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToSingle<TModel>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToList<TModel>();
            });
        }

        public SelectMapper<TModel> Select<T>(Expression<Func<TModel, T, dynamic>> fields = null) where T : EntityBase, new()
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return this;
        }

        public SelectMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return this;
        }

        public SelectMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public SelectMapper<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);
            return this;
        }

        public SelectMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public SelectMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public SelectMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public SelectMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
          where TLeft : EntityBase, new()
          where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public SelectMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : EntityBase, new()
            where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public SelectMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : EntityBase, new()
            where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public SelectMapper<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : EntityBase, new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return this;
        }

        public SelectMapper<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : EntityBase, new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return this;
        }

        private RawExecuteResult InternalExecuteSql()
        {
            Handler<TModel> builder = new SelectHandler<TModel>(_segmentManager);
            var translationResult = builder.GetTranslationResult();
            return translationResult.ExecuteTranslateResult(_executionCore);
        }
    }
}
