using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class InternalSqlContext : IDisposable
    {
        private Boolean _disposed = false;

        private Boolean _useTransaction;

        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private readonly ILogger _logger;

        internal InternalSqlContext()
        {
            _connection = SwitchDatabase.GetConnectionInstance();

            _logger = new ConsoleLogger(this);
            _logger.Write("INFO", $@"datastore init connectionstring:{SwitchDatabase.ConnectionString}");
        }

        public void OpenTransaction()
        {
            _logger.Write("INFO", "open transaction");
            _useTransaction = true;
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

        internal TemporaryMarshalValue Execute(ExecuteType executeType, String sql, IEnumerable<SqlParameterMapper> parameters = null, CommandType commandType = CommandType.Text)
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
                var temporaryMarshalValue = new TemporaryMarshalValue();

                if (executeType == ExecuteType.SELECT)
                {
                    using (cmd.ExecuteReader())
                    {
                        var dr = cmd.ExecuteReader();
                        var dataTable = new DataTable("tmpDt");
                        dataTable.Load(dr, LoadOption.Upsert);
                        temporaryMarshalValue.MarshalValue = dataTable;
                    }
                }
                else if (executeType == ExecuteType.UPDATE)
                {
                    var count = Int32.Parse(cmd.ExecuteNonQuery().ToString());
                    temporaryMarshalValue.MarshalValue = count;
                }
                else if (executeType == ExecuteType.INSERT)
                {
                    var count = Int32.Parse(cmd.ExecuteScalar().ToString());
                    temporaryMarshalValue.MarshalValue = count;
                }
                cmd.Parameters.Clear();
                return temporaryMarshalValue;
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


    public enum ExecuteType
    {
        SELECT = 1,
        UPDATE = 2,
        INSERT = 3
    }
}
