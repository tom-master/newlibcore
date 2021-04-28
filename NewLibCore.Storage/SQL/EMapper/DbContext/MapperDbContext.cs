﻿using System;
using System.Data;
using System.Data.Common;
using System.Linq;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 执行解析后的SQL
    /// </summary>
    internal sealed class MapperDbContext : MapperDbContextBase
    {
        private Boolean _disposed = false;

        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private readonly EntityMapperOptions _options;

        /// <summary>
        /// 初始化MapperDbContext类的新实例
        /// </summary>
        public MapperDbContext(EntityMapperOptions options)
        {
            Check.IfNullOrZero(options);
            _options = options;
            _connection = options.TemplateBase.CreateDbConnection();
        }

        protected internal override void Commit()
        {
            if (_dataTransaction != null)
            {
                _dataTransaction.Commit();
                RunDiagnosis.Info("提交事务");
            }
        }

        protected internal override void Rollback()
        {
            if (_dataTransaction != null)
            {
                _dataTransaction.Rollback();
                RunDiagnosis.Info("事务回滚");
            }
        }

        protected internal override void OpenConnection()
        {
            if (_connection.State == ConnectionState.Closed)
            {
                RunDiagnosis.Info("开启连接");
                _connection.ConnectionString = ConfigReader.GetHostVar(_options.ConnectionStringName);
                _connection.Open();
            }
        }

        protected internal override DbTransaction OpenTransaction()
        {
            if (_dataTransaction == null)
            {
                _dataTransaction = _connection.BeginTransaction(_options.TransactionLevel);
                RunDiagnosis.Info("开启事务");
            }
            return _dataTransaction;
        }

        protected internal override void Dispose(Boolean disposing)
        {
            RunDiagnosis.Info($@"释放资源{Environment.NewLine}");
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
                    _dataTransaction = null;
                }
                _disposed = true;
            }
        }

        protected internal override ExecutorResult Insert(String sql, params MapperParameter[] parameters)
        {
            Check.IfNullOrZero(sql);
            OpenConnection();
            using (var cmd = _connection.CreateCommand())
            {
                if (UseTransaction)
                {
                    cmd.Transaction = OpenTransaction();
                }
                cmd.CommandType = CommandType.Text;
                cmd.CommandText = sql;
                if (parameters != null && parameters.Any())
                {
                    cmd.Parameters.AddRange(parameters.Select(s => _options.TemplateBase.CreateParameter(s.Key, s.Value, s.RuntimeType)).ToArray());
                }
                RunDiagnosis.Info($@"SQL语句:{sql} 占位符与参数:{(parameters == null || !parameters.Any() ? "" : String.Join($@"{Environment.NewLine}", parameters.Select(s => $@"{s.Key}----{s.Value}")))}");

                var executeResult = new ExecutorResult();
                executeResult.SaveRawResult(cmd.ExecuteScalar());
                cmd.Parameters.Clear();
                return executeResult;
            }
        }

        protected internal override ExecutorResult Update(String sql, params MapperParameter[] parameters)
        {
            Check.IfNullOrZero(sql);
            OpenConnection();
            using (var cmd = _connection.CreateCommand())
            {
                if (UseTransaction)
                {
                    cmd.Transaction = OpenTransaction();
                }
                cmd.CommandType = CommandType.Text;
                cmd.CommandText = sql;
                if (parameters != null && parameters.Any())
                {
                    cmd.Parameters.AddRange(parameters.Select(s => _options.TemplateBase.CreateParameter(s.Key, s.Value, s.RuntimeType)).ToArray());
                }
                RunDiagnosis.Info($@"SQL语句:{sql} 占位符与参数:{(parameters == null || !parameters.Any() ? "" : String.Join($@"{Environment.NewLine}", parameters.Select(s => $@"{s.Key}----{s.Value}")))}");

                var executeResult = new ExecutorResult();
                executeResult.SaveRawResult(cmd.ExecuteNonQuery());
                cmd.Parameters.Clear();
                return executeResult;
            }
        }

        protected internal override ExecutorResult Select(String sql, params MapperParameter[] parameters)
        {
            Check.IfNullOrZero(sql);
            OpenConnection();
            using (var cmd = _connection.CreateCommand())
            {
                if (UseTransaction)
                {
                    cmd.Transaction = OpenTransaction();
                }
                cmd.CommandType = CommandType.Text;
                cmd.CommandText = sql;
                if (parameters != null && parameters.Any())
                {
                    cmd.Parameters.AddRange(parameters.Select(s => _options.TemplateBase.CreateParameter(s.Key, s.Value, s.RuntimeType)).ToArray());
                }
                RunDiagnosis.Info($@"SQL语句:{sql} 占位符与参数:{(parameters == null || !parameters.Any() ? "" : String.Join($@"{Environment.NewLine}", parameters.Select(s => $@"{s.Key}----{s.Value}")))}");

                using (var dr = cmd.ExecuteReader())
                {
                    var dataTable = new DataTable("tmpDt");
                    dataTable.Load(dr, LoadOption.Upsert);

                    var executeResult = new ExecutorResult();
                    executeResult.SaveRawResult(dataTable);
                    cmd.Parameters.Clear();
                    return executeResult;
                }
            }
        }
    }
}
