using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Execute
{
    public sealed class ExecuteCore : IDisposable
    {
        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private Boolean _disposed = false;

        private Boolean _useTransaction = false;

        internal ExecuteCore()
        {
            Parameter.Validate(MapperFactory.Mapper);
            _connection = MapperFactory.Mapper.GetConnectionInstance();
        }

        internal void OpenTransaction()
        {
            MapperFactory.Logger.Write("INFO", "open transaction");
            _useTransaction = true;
        }

        internal void Commit()
        {
            if (_useTransaction)
            {
                if (_dataTransaction != null)
                {
                    _dataTransaction.Commit();
                    MapperFactory.Logger.Write("INFO", "commit transaction");
                }
                return;
            }
            throw new Exception("没有启动事务，无法执行事务提交");
        }

        internal void Rollback()
        {
            if (_useTransaction)
            {
                if (_dataTransaction != null)
                {
                    _dataTransaction.Rollback();
                    MapperFactory.Logger.Write("INFO", "rollback transaction ");
                }
                return;
            }
            throw new Exception("没有启动事务，无法执行事务回滚");
        }

        internal ExecuteCoreResult Execute(ExecuteType executeType, TranslationCoreResult translationCore)
        {
            Parameter.Validate(translationCore);
            return Execute(executeType, translationCore.GetSql(), translationCore.GetParameters(), CommandType.Text);
        }

        internal ExecuteCoreResult Execute(ExecuteType executeType, String sql, IEnumerable<EntityParameter> parameters = null, CommandType commandType = CommandType.Text)
        {
            try
            {
                Parameter.Validate(sql);
                sql = ReformatSql(sql);
                if ((executeType == ExecuteType.SELECT || executeType == ExecuteType.SELECT_SINGLE) && MapperFactory.Cache != null)
                {
                    var md5 = MD.GetMD5(PrepareCacheKey(sql, parameters));
                    var cacheResult = MapperFactory.Cache.Get(md5);
                    if (cacheResult != null)
                    {
                        MapperFactory.Logger.Write("INFO", "return from cache");
                        return (ExecuteCoreResult)cacheResult;
                    }
                }

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
                    MapperFactory.Logger.Write("INFO", $@"ExecuteType:{executeType}");
                    MapperFactory.Logger.Write("INFO", $@"SQL:{sql}");
                    MapperFactory.Logger.Write("INFO", $@"PARAMETERS:{(parameters == null || !parameters.Any() ? "" : String.Join($@"{Environment.NewLine}", parameters.Select(s => $@"{s.Key}----{s.Value}")))}");
                    var executeResult = new ExecuteCoreResult();
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

                    if ((executeType == ExecuteType.SELECT || executeType == ExecuteType.SELECT_SINGLE) && MapperFactory.Cache != null)
                    {
                        MapperFactory.Logger.Write("INFO", "add to cache");
                        MapperFactory.Cache.Add(MD.GetMD5(PrepareCacheKey(sql, parameters)), executeResult);
                    }

                    return executeResult;
                }
            }
            catch (Exception ex)
            {
                MapperFactory.Logger.Write("ERROR", $@"{ex}");
                throw;
            }
        }

        private static String PrepareCacheKey(String sql, IEnumerable<EntityParameter> parameters)
        {
            Parameter.Validate(sql);
            var cacheKey = sql;
            foreach (var item in parameters)
            {
                cacheKey = cacheKey.Replace(item.Key, item.Value.ToString());
            }

            return cacheKey;
        }

        private String ReformatSql(String sql)
        {
            Parameter.Validate(sql);
            sql = sql.Replace("  ", " ");
            return sql;
        }

        private void Open()
        {
            if (_connection.State == ConnectionState.Closed)
            {
                MapperFactory.Logger.Write("INFO", "open connection");
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
                    MapperFactory.Logger.Write("INFO", "begin transaction");
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
            MapperFactory.Logger.Write("INFO", $@"close connection {Environment.NewLine}");
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
