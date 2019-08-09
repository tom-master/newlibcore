using System;
using System.Collections.Generic;
using System.Data;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.InternalHandler;
using NewLibCore.Data.SQL.Mapper.InternalMapper;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper
    {
        private EntityMapper() { }

        /// <summary>
        /// 创建一个EntityMapper实例
        /// </summary>
        /// <returns></returns>
        public static EntityMapper CreateMapper()
        {
            return new EntityMapper();
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

            Handler<TModel> builder = new InsertHandler<TModel>(model, true);
            var executeResult = builder.GetExecuteResult();
            Int32.TryParse(executeResult.Value.ToString(), out var modelId);
            model.Id = modelId;
            return model;
        }

        /// <summary>
        /// 修改一個TModel
        /// </summary>
        /// <param name="model"></param>
        /// <param name="expression"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);
            Parameter.Validate(expression);

            var segmentManager = new SegmentManager();
            segmentManager.Add(expression);
            Handler<TModel> builder = new UpdateHandler<TModel>(model, segmentManager, true);
            var translateResult = builder.GetExecuteResult();
            return (Int32)translateResult.Value > 0;
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <param name="fields"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public SearchMapper<TModel> Select<TModel>(Expression<Func<TModel, dynamic>> fields = null) where TModel : EntityBase, new()
        {
            return new SearchMapper<TModel>().Select(fields);
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <param name="fields"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public SearchMapper<TModel> Select<TModel, T>(Expression<Func<TModel, T, dynamic>> fields = null) where TModel : EntityBase, new()
        where T : EntityBase, new()
        {
            return new SearchMapper<TModel>().Select(fields);
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
            var dataTable = (DataTable)RawExecute(ExecuteType.SELECT, sql, parameters).Value;
            return dataTable.ToList<TModel>();
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

            var modelType = typeof(TModel);
            RawExecuteResult executeResult;
            if (modelType.IsNumeric())
            {
                executeResult = RawExecute(ExecuteType.SELECT_SINGLE, sql, parameters);
                return (TModel)Convert.ChangeType(executeResult.Value, modelType);
            }

            executeResult = RawExecute(ExecuteType.SELECT, sql, parameters);
            var dataTable = (DataTable)executeResult.Value;
            return dataTable.ToSingle<TModel>();
        }

        /// <summary>
        /// 直接执行sql语句
        /// </summary>
        /// <param name="executeType"></param>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <returns></returns>
        private RawExecuteResult RawExecute(ExecuteType executeType, String sql, IEnumerable<EntityParameter> parameters = null)
        {
            var sqlResult = new SqlResult
            {
                ExecuteType = executeType
            };
            sqlResult.Append(sql, parameters);
            return sqlResult.GetExecuteResult();
        }
    }
}
