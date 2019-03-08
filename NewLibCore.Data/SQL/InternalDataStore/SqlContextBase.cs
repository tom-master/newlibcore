using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public abstract class SqlContextBase : IDisposable
    {
        private Boolean _disposed = false;

        private Boolean _useTransaction;

        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private readonly Boolean _noExecuteMode = false;

        private readonly ILogger _logger;

        public SqlContextBase(String connection)
        {
            _noExecuteMode = String.IsNullOrEmpty(connection);

            _connection = new MySqlConnection(connection);
            _logger = new ConsoleLogger(this);
            _logger.Write("INFO", $@"datastore init connectionstring:{connection}");
        }

        public void OpenTransaction()
        {
            _logger.Write("INFO", "open transaction");
            _useTransaction = true;
        }

        private DbTransaction GetNonceTransaction()
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

        public void Commit()
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

        public void Rollback()
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

        private void Open()
        {
            if (_connection.State == ConnectionState.Closed)
            {
                _logger.Write("INFO", "open connection");
                _connection.Open();
            }
        }

        protected TemporaryMarshalValue Execute(String sqlStr, IEnumerable<SqlParameterMapper> parameters = null, CommandType commandType = CommandType.Text)
        {
            if (_noExecuteMode)
            {
                return null;
            }

            Open();
            using (var cmd = _connection.CreateCommand())
            {
                if (_useTransaction)
                {
                    cmd.Transaction = GetNonceTransaction();
                }
                cmd.CommandType = commandType;
                cmd.CommandText = sqlStr;
                if (parameters != null && parameters.Any())
                {
                    cmd.Parameters.AddRange(parameters.Select(s => (DbParameter)s).ToArray());
                }
                var temporaryMarshalValue = new TemporaryMarshalValue();
                InternalExecute(cmd, temporaryMarshalValue);
                return temporaryMarshalValue;
            }
        }

        protected abstract void InternalExecute(DbCommand dbCommand, TemporaryMarshalValue temporaryMarshalValue);

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
