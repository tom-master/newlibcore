using System;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.InternalHandler;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;
using Newtonsoft.Json;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {
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
        public SelectMapper<TModel> Select<TModel>(Expression<Func<TModel, dynamic>> fields = null) where TModel : EntityBase, new()
        {
            return new SelectMapper<TModel>().Select(fields);
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <param name="fields"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public SelectMapper<TModel> Select<TModel, T>(Expression<Func<TModel, T, dynamic>> fields = null) where TModel : EntityBase, new()
        where T : EntityBase, new()
        {
            return new SelectMapper<TModel>().Select(fields);
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

        public void Dispose()
        {
        }
    }

    public sealed class SelectMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly SegmentManager _segmentManager = new SegmentManager();

        internal SelectMapper() { }

        public Boolean Exist()
        {
            return Count() > 0;
        }

        public Int32 Count()
        {
            return Watch<Int32>(() =>
            {
                Select(s => "COUNT(*)");
                var executeResult = InternalExecuteSql();
                Int32.TryParse(executeResult.Value.ToString(), out var count);
                return count;
            });
        }

        public TModel FirstOrDefault()
        {
            return Watch<TModel>(() =>
            {
                var executeResult = InternalExecuteSql();
                var dataTable = executeResult.Value as DataTable;
                return dataTable.ToSingle<TModel>();
            });
        }

        public List<TModel> ToList()
        {
            return Watch<List<TModel>>(() =>
            {
                var executeResult = InternalExecuteSql();
                var dataTable = executeResult.Value as DataTable;
                return dataTable.ToList<TModel>();
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
            var executeResult = builder.GetExecuteResult();
            MapperConfig.Instance.Logger.Info($@"查询后的结果:{JsonConvert.SerializeObject(executeResult.Value)}");
            return executeResult;
        }

        private T Watch<T>(Func<Object> func)
        {
            var sw = new Stopwatch();
            sw.Start();
            var returnValue = (T)func();
            sw.Stop();
            MapperConfig.Instance.Logger.Info($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 2)}s");

            return returnValue;
        }
    }
}
