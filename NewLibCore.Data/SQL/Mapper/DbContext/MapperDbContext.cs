using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 执行解析后的SQL
    /// </summary>
    internal sealed class MapperDbContext : MapperDbContextBase
    {

        private Boolean _disposed = false;

        private DbConnection _connection;

        private DbTransaction _dataTransaction;

        private readonly TemplateBase _templateBase;

        /// <summary>
        /// 初始化MapperDbContext类的新实例
        /// </summary>
        public MapperDbContext(IServiceProvider serviceProvider)
        {
            _templateBase = serviceProvider.GetService<TemplateBase>();
            _connection = _templateBase.CreateDbConnection();
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
                _connection.Open();
            }
        }

        protected internal override DbTransaction OpenTransaction()
        {
            if (_dataTransaction == null)
            {
                _dataTransaction = _connection.BeginTransaction(MapperConfig.TransactionLevel);
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

        protected internal override ExecuteType GetExecuteType(String sql)
        {
            Parameter.Validate(sql);

            var operationType = sql.Substring(0, sql.IndexOf(" "));
            if (Enum.TryParse<ExecuteType>(operationType, out var executeType))
            {
                return executeType;
            }

            throw new Exception($@"SQL语句执行类型解析失败:{operationType}");
        }

        protected internal override ExecuteResult RawExecute(String sql, IEnumerable<MapperParameter> parameters = null)
        {
            try
            {
                Parameter.Validate(sql);
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
                        cmd.Parameters.AddRange(parameters.Select(s => s.ConvertToDbParameter(_templateBase)).ToArray());
                    }
                    RunDiagnosis.Info($@"SQL语句:{sql} 占位符与参数:{(parameters == null || !parameters.Any() ? "" : String.Join($@"{Environment.NewLine}", parameters.Select(s => $@"{s.Key}----{s.Value}")))}");

                    var executeType = GetExecuteType(sql);
                    var executeResult = new ExecuteResult();
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
                RunDiagnosis.Error($@"SQL语句执行发生异常:{ex}");
                throw;
            }
        }
    }
}
