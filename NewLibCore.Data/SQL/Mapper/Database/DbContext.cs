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
    internal sealed class MapperDbContext : IMapperDbContext
    {
        private Boolean _disposed = false;
        private DbConnection _connection;
        private DbTransaction _dataTransaction;

        public MapperDbContext()
        {
            var instanceConfig = MapperConfig.ServiceProvider.GetService<InstanceConfig>();
            _connection = instanceConfig.GetConnectionInstance();
        }

        public void Commit()
        {
            if (_dataTransaction != null)
            {
                _dataTransaction.Commit();
                RunDiagnosis.Info("提交事务");
            }
        }

        public void Rollback()
        {
            if (_dataTransaction != null)
            {
                _dataTransaction.Rollback();
                RunDiagnosis.Info("事务回滚");
            }
        }

        public void OpenConnection()
        {
            if (_connection.State == ConnectionState.Closed)
            {
                RunDiagnosis.Info("开启连接");
                _connection.Open();
            }
        }

        public void OpenTransaction()
        {
            if (_dataTransaction == null)
            {
                _dataTransaction = _connection.BeginTransaction();
                RunDiagnosis.Info("开启事务");
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        public void Dispose(Boolean disposing)
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
                    _dataTransaction = null;
                }
                _disposed = true;
            }
        }

        public ExecuteType GetExecuteType(String sql)
        {
            Parameter.Validate(sql);

            var operationType = sql.Substring(0, sql.IndexOf(" "));
            if (Enum.TryParse<ExecuteType>(operationType, out var executeType))
            {
                return executeType;
            }
            throw new Exception($@"SQL语句执行类型解析失败:{operationType}");
        }

        public RawExecuteResult RawExecute(String sql, IEnumerable<EntityParameter> parameters = null, CommandType commandType = CommandType.Text)
        {
            try
            {
                Parameter.Validate(sql);
                OpenConnection();
                using (var cmd = _connection.CreateCommand())
                {
                    if (_dataTransaction != null)
                    {
                        cmd.Transaction = _dataTransaction;
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

    }
}
