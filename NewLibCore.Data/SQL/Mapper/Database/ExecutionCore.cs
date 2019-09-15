using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Database
{
    /// <summary>
    /// sql语句执行
    /// </summary>
    internal sealed class ExecutionCore : IDisposable
    {
        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private Boolean _disposed = false;

        private Boolean _useTransaction = false;

        /// <summary>
        /// 初始化一个ExecutionCore类的实例
        /// </summary>
        public ExecutionCore()
        {
            var instanceConfig = MapperConfig.ServiceProvider.GetService<InstanceConfig>();
            Parameter.Validate(instanceConfig);
            _connection = instanceConfig.GetConnectionInstance();
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
        /// <param name="executeType">执行的类型</param>
        /// <param name="translationCore">表达式翻译sql语句</param>
        /// <returns></returns>
        internal RawExecuteResult Execute(TranslationResult tanslationResult)
        {
            Parameter.Validate(tanslationResult);
            return RawExecute(tanslationResult.ToString(), tanslationResult.GetParameters(), CommandType.Text);
        }

        /// <summary>
        /// 执行原生sql语句
        /// </summary>
        /// <param name="executeType">执行的类型</param>
        /// <param name="sql">语句</param>
        /// <param name="parameters">参数</param>
        /// <param name="commandType"></param>
        /// <returns></returns>
        internal RawExecuteResult RawExecute(String sql, IEnumerable<EntityParameter> parameters = null, CommandType commandType = CommandType.Text)
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

                    var executeType = GetExecuteType(sql);
                    var executeResult = new RawExecuteResult();
                    if (executeType == ExecuteType.SELECT)
                    {
                        using (var dr = cmd.ExecuteReader())
                        {
                            var dataTable = new DataTable("tmpDt");
                            dataTable.Load(dr, LoadOption.Upsert);
                            executeResult.SaveRawResult(dataTable);
                        }
                    }
                    else if (executeType == ExecuteType.UPDATE)
                    {
                        executeResult.SaveRawResult(cmd.ExecuteNonQuery());
                    }
                    else if (executeType == ExecuteType.INSERT)
                    {
                        executeResult.SaveRawResult(cmd.ExecuteScalar());
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

        /// <summary>
        /// 释放资源
        /// </summary>
        /// <param name="disposing"></param>
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

        /// <summary>
        /// 获取语句的执行类型
        /// </summary>
        /// <param name="sql">语句</param>
        /// <returns></returns>
        internal ExecuteType GetExecuteType(String sql)
        {
            Parameter.Validate(sql);

            var operationType = sql.Substring(0, sql.IndexOf(" "));
            if (Enum.TryParse<ExecuteType>(operationType, out var executeType))
            {
                return executeType;
            }
            throw new Exception($@"SQL语句执行类型解析失败:{operationType}");
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
    }
}
