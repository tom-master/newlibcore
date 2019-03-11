using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.DataExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class SqlContext : IDisposable
    {
        private Boolean _disposed = false;

        private Boolean _useTransaction;

        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private readonly Boolean _noExecuteMode = false;

        private readonly ILogger _logger;

        public SqlContext(String connection)
        {
            _noExecuteMode = String.IsNullOrEmpty(connection);
            _connection = new MySqlConnection(connection);
            _logger = new ConsoleLogger(this);
            _logger.Write("INFO", $@"datastore init connectionstring:{connection}");
        }

        public TModel Add<TModel>(TModel model) where TModel : DomainModelBase, new()
        {
            BuilderBase<TModel> builder = new AddBuilder<TModel>(model, true);
            var entry = builder.Build();
            var returnValue = Execute(ExecuteType.INSERT, entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            model.Id = (Int32)returnValue.MarshalValue;
            return model;
        }

        public Boolean Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new ModifyBuilder<TModel>(model, where, true);
            var entry = builder.Build();
            var returnValue = Execute(ExecuteType.UPDATE, entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            return (Int32)returnValue.MarshalValue > 0;
        }

        public IList<TModel> Find<TModel>(Expression<Func<TModel, Boolean>> where, Expression<Func<TModel, dynamic>> fields = null) where TModel : PropertyMonitor, new()
        {
            BuilderBase<TModel> builder = new SelectBuilder<TModel>(where, fields);
            var entry = builder.Build();
            var returnValue = Execute(ExecuteType.SELECT, entry.SqlStore.ToString(), entry.ParameterStore, CommandType.Text);
            var dataTable = returnValue.MarshalValue as DataTable;
            return dataTable.AsList<TModel>();
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

        private TemporaryMarshalValue Execute(ExecuteType executeType, String sqlStr, IEnumerable<SqlParameterMapper> parameters = null, CommandType commandType = CommandType.Text)
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

    internal enum ExecuteType
    {
        SELECT = 1,
        UPDATE = 2,
        INSERT = 3
    }
}
