using NewLibCore.Data.SQL.InternalTranslation;
using NewLibCore.Data.SQL.MapperConfig;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;

namespace NewLibCore.Data.SQL.InternalExecute
{
    internal sealed class ExecuteContext : IDisposable
    {
        private readonly ILogger _logger;

        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private Boolean _disposed = false;

        private Boolean _useTransaction = false;

        internal ExecuteContext()
        {
            _connection = SqlMapperConfig.GetConnectionInstance();

            _logger = new ConsoleLogger(this);
            _logger.Write("INFO", $@"datastore init connectionstring:{SqlMapperConfig.ConnectionString}");
        }

        internal void OpenTransaction()
        {
            _logger.Write("INFO", "open transaction");
            _useTransaction = true;
        }

        internal void Commit()
        {
            if (_useTransaction)
            {
                _logger.Write("INFO", "commit transaction");
                if (_dataTransaction != null)
                {
                    _dataTransaction.Commit();
                }
                return;
            }
            throw new Exception("没有启动事务，无法执行事务提交");
        }

        internal void Rollback()
        {
            if (_useTransaction)
            {
                _logger.Write("INFO", "rollback transaction ");
                if (_dataTransaction != null)
                {
                    _dataTransaction.Rollback();
                }
                return;
            }
            throw new Exception("没有启动事务，无法执行事务回滚");
        }

        internal ExecuteResult Execute(ExecuteType executeType, String sql, IEnumerable<EntityParameter> parameters = null, CommandType commandType = CommandType.Text)
        {
            try
            {
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

                    var executeResult = new ExecuteResult();
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
                    else if (executeType == ExecuteType.INSERT || executeType == ExecuteType.SELECTSINGLE)
                    {
                        executeResult.Value = cmd.ExecuteScalar();
                    }
                    cmd.Parameters.Clear();
                    return executeResult;
                }
            }
            catch (Exception ex)
            {
                _logger.Write("ERROR", $@"{ex}:{sql}:parameters{(parameters == null ? "" : String.Join(",", parameters.Select(s => $@"{s.Key}--{s.Value}")))}");
                throw;
            }
        }

        private void Open()
        {
            if (_connection.State == ConnectionState.Closed)
            {
                _logger.Write("INFO", "open connection");
                _connection.Open();
            }
        }

        private DbTransaction BeginTransaction()
        {
            if (_useTransaction)
            {
                if (_dataTransaction == null)
                {
                    _useTransaction = true;
                    _dataTransaction = _connection.BeginTransaction();
                }
                return _dataTransaction;
            }
            throw new Exception("没有启动事务");
        }

        #region dispose

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        private void Dispose(Boolean disposing)
        {
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

        #endregion
    }

}
