using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.MapperExtension;
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
                 Handler handler = new InsertHandler<TModel>(model, true);
                 var translationResult = handler.GetTranslationResult();
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
                 var segmentManager = MapperConfig.ServiceProvider.GetService<SegmentManager>();
                 segmentManager.Add(expression);
                 Handler handler = new UpdateHandler<TModel>(model, segmentManager, true);
                 return handler.GetTranslationResult().ExecuteTranslateResult(_executionCore).ToPrimitive<Int32>() > 0;
             });
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <param name="fields">字段</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public SelectMapper<TModel> From<TModel>() where TModel : new()
        {
            return new SelectMapper<TModel>(_executionCore).From();
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


}
