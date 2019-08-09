using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Database
{
    /// <summary>
    /// sql语句执行
    /// </summary>
    public class ExecutionCore : IDisposable
    {
        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private Boolean _disposed = false;

        private Boolean _useTransaction = false;

        internal ExecutionCore()
        {
            Parameter.Validate(MapperConfig.DatabaseConfig);
            _connection = MapperConfig.DatabaseConfig.GetConnectionInstance();
        }

        /// <summary>
        /// 将表达式翻译结果执行
        /// </summary>
        /// <param name="executeType"></param>
        /// <param name="translationCore"></param>
        /// <returns></returns>
        internal RawExecuteResult Execute(SqlResult sqlResult)
        {
            Parameter.Validate(sqlResult);
            return RawExecute(sqlResult.ExecuteType, sqlResult.GetSql(), sqlResult.GetParameters(), CommandType.Text);
        }

        /// <summary>
        /// 执行原生sql语句
        /// </summary>
        /// <param name="executeType"></param>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        /// <param name="commandType"></param>
        /// <returns></returns>
        internal RawExecuteResult RawExecute(ExecuteType executeType, String sql, IEnumerable<EntityParameter> parameters = null, CommandType commandType = CommandType.Text)
        {
            try
            {
                Parameter.Validate(sql);
                Open();
                using (var cmd = _connection.CreateCommand())
                {
                    if (_useTransaction)
                    {
                        cmd.Transaction = BeginTransaction();
                    }
                    cmd.CommandType = commandType;
                    cmd.CommandText = sql;
                    if (parameters != null && parameters.Any())
                    {
                        cmd.Parameters.AddRange(parameters.Select(s => (DbParameter)s).ToArray());
                    }
                    MapperConfig.DatabaseConfig.Logger.Info($@"SQL语句:{sql} 占位符与参数:{(parameters == null || !parameters.Any() ? "" : String.Join($@"{Environment.NewLine}", parameters.Select(s => $@"{s.Key}----{s.Value}")))}");

                    var executeResult = new RawExecuteResult();
                    if (executeType == ExecuteType.SELECT)
                    {
                        using (var dr = cmd.ExecuteReader())
                        {
                            var dataTable = new DataTable("tmpDt");
                            dataTable.Load(dr, LoadOption.Upsert);
                            executeResult.Value = dataTable;
                        }
                    }
                    else if (executeType == ExecuteType.UPDATE)
                    {
                        executeResult.Value = cmd.ExecuteNonQuery();
                    }
                    else if (executeType == ExecuteType.INSERT || executeType == ExecuteType.SELECT_SINGLE)
                    {
                        executeResult.Value = cmd.ExecuteScalar();
                    }
                    cmd.Parameters.Clear();
                    return executeResult;
                }
            }
            catch (Exception ex)
            {
                MapperConfig.DatabaseConfig.Logger.Error($@"{ex}");
                throw;
            }
        }

        /// <summary>
        /// 打开实例连接
        /// </summary>
        private void Open()
        {
            if (_connection.State == ConnectionState.Closed)
            {
                MapperConfig.DatabaseConfig.Logger.Info("开启连接");
                _connection.Open();
            }
        }

        /// <summary>
        /// 事物起始
        /// </summary>
        /// <returns></returns>
        private DbTransaction BeginTransaction()
        {
            if (_useTransaction)
            {
                if (_dataTransaction == null)
                {
                    _useTransaction = true;
                    _dataTransaction = _connection.BeginTransaction();
                    MapperConfig.DatabaseConfig.Logger.Info("开启事务");
                }
                return _dataTransaction;
            }
            throw new Exception("没有启动事务");
        }


        /// <summary>
        /// 开启一个事物
        /// </summary>
        internal void OpenTransaction()
        {
            _useTransaction = true;
        }

        /// <summary>
        /// 提交一个事物
        /// </summary>
        internal void Commit()
        {
            if (_useTransaction)
            {
                if (_dataTransaction != null)
                {
                    _dataTransaction.Commit();
                    MapperConfig.DatabaseConfig.Logger.Info("提交事务");
                }
                return;
            }
            throw new Exception("没有启动事务，无法执行事务提交");
        }

        /// <summary>
        /// 回滚一个事物
        /// </summary>
        internal void Rollback()
        {
            if (_useTransaction)
            {
                if (_dataTransaction != null)
                {
                    _dataTransaction.Rollback();
                    MapperConfig.DatabaseConfig.Logger.Info("事务回滚");
                }
                return;
            }
            throw new Exception("没有启动事务，无法执行事务回滚");
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        private void Dispose(Boolean disposing)
        {
            MapperConfig.DatabaseConfig.Logger.Info($@"关闭连接{Environment.NewLine}");
            if (!_disposed)
            {
                if (!disposing)
                {
                    return;
                }

                if (_connection != null)
                {
                    if (_connection.State != ConnectionState.Closed)
                    {
                        _connection.Close();
                    }
                    _connection.Dispose();
                    _connection = null;
                }
                _disposed = true;
            }
        }
    }
}
