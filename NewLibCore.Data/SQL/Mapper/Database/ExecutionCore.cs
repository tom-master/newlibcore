using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Database
{
    /// <summary>
    /// sql语句执行
    /// </summary>
    internal class ExecutionCore : IDisposable
    {
        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private Boolean _disposed = false;

        private Boolean _useTransaction = false;

        private ExecutionCore()
        {
            Parameter.Validate(MapperConfig.Instance);
            _connection = MapperConfig.Instance.GetConnectionInstance();
        }

        internal static ExecutionCore CreateExecutionCore()
        {
            return new ExecutionCore();
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
                    RunDiagnosis.Info("提交事务");
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
                    RunDiagnosis.Info("事务回滚");
                }
                return;
            }
            throw new Exception("没有启动事务，无法执行事务回滚");
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
            return RawExecute(sqlResult.ExecuteType, sqlResult.ToString(), sqlResult.GetParameters(), CommandType.Text);
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
                    RunDiagnosis.Info($@"SQL语句:{sql} 占位符与参数:{(parameters == null || !parameters.Any() ? "" : String.Join($@"{Environment.NewLine}", parameters.Select(s => $@"{s.Key}----{s.Value}")))}");

                    var executeResult = new RawExecuteResult();
                    if (executeType == ExecuteType.SELECT)
                    {
                        using (var dr = cmd.ExecuteReader())
                        {
                            var dataTable = new DataTable("tmpDt");
                            dataTable.Load(dr, LoadOption.Upsert);
                            executeResult.SetRawResult(dataTable);
                        }
                    }
                    else if (executeType == ExecuteType.UPDATE)
                    {
                        executeResult.SetRawResult(cmd.ExecuteNonQuery());
                    }
                    else if (executeType == ExecuteType.INSERT || executeType == ExecuteType.SELECT_SINGLE)
                    {
                        executeResult.SetRawResult(cmd.ExecuteScalar());
                    }

                    cmd.Parameters.Clear();
                    return executeResult;
                }
            }
            catch (Exception ex)
            {
                RunDiagnosis.Error($@"{ex}");
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
                RunDiagnosis.Info("开启连接");
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
                    RunDiagnosis.Info("开启事务");
                }
                return _dataTransaction;
            }
            throw new Exception("没有启动事务");
        }

        private void Dispose(Boolean disposing)
        {
            RunDiagnosis.Info($@"关闭连接{Environment.NewLine}");
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

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
    }
}
