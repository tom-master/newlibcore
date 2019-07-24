using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.OperationProvider;
using NewLibCore.Data.SQL.Mapper.OperationProvider.Imp;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {
        private readonly ExecutionCore _executionCore;

        public EntityMapper()
        {
            _executionCore = new ExecutionCore();
        }

        /// <summary>
        /// 添加一個TModel
        /// </summary>
        /// <param name="model"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);
            return new AddMapper<TModel>(_executionCore).Add(model);
        }

        /// <summary>
        /// 修改一個TModel
        /// </summary>
        /// <param name="model"></param>
        /// <param name="expression"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);
            Parameter.Validate(expression);
            return new ModifyMapper<TModel>(_executionCore).Update(model, expression);
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <param name="fields"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public ISearchMapper<TModel> Select<TModel>(Expression<Func<TModel, dynamic>> fields = null) where TModel : EntityBase, new()
        {
            return new SearchMapper<TModel>(_executionCore).Select(fields);
        }

        /// <summary>
        /// 查詢一個TModel
        /// </summary>
        /// <param name="fields"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public ISearchMapper<TModel> Select<TModel, T>(Expression<Func<TModel, T, dynamic>> fields = null) where TModel : EntityBase, new()
        where T : EntityBase, new()
        {
            return new SearchMapper<TModel>(_executionCore).Select(fields);
        }

        /// <summary>
        /// 执行一個返回列表的sql语句
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public List<TModel> ExecuteToList<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            Parameter.Validate(sql);
            return new RawExecutor(_executionCore).ToList<TModel>(sql, parameters);
        }

        /// <summary>
        /// 执行一个返回单个TModel的sql语句
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel ExecuteToSingle<TModel>(String sql, IEnumerable<EntityParameter> parameters = null) where TModel : new()
        {
            Parameter.Validate(sql);
            return new RawExecutor(_executionCore).ToSingle<TModel>(sql, parameters);
        }

        /// <summary>
        /// 开启事物
        /// </summary>
        public void OpenTransaction()
        {
            _executionCore.OpenTransaction();
        }
        /// <summary>
        /// 提交事物
        /// </summary>
        public void Commit()
        {
            _executionCore.Commit();
        }

        /// <summary>
        /// 回滚事物
        /// </summary>
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
