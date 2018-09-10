using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Linq.Expressions;
using MySql.Data.MySqlClient;
using NewLibCore.Data.Mapper.DataExtension;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper.InternalDataStore
{
	public class DataStore : IDisposable
	{
		private DbConnection _connection;

		private DbTransaction _dataTransaction;

		private Boolean disposed = false;

		private Boolean _useTransaction;

		private Boolean _noExecuteMode = false;

		public DataStore(String connection, Boolean noExecuteMode = false)
		{
			_connection = new MySqlConnection(connection);
			_noExecuteMode = noExecuteMode;
		}

		public void OpenTransaction()
		{
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
				_dataTransaction.Commit();
				return;
			}
			throw new Exception("没有启动事务，无法执行事务提交");
		}

		public void Rollback()
		{
			if (_useTransaction)
			{
				_dataTransaction?.Rollback();
				return;
			}
			throw new Exception("没有启动事务，无法执行事务回滚");
		}

		private void Open()
		{
			if (_connection.State == ConnectionState.Closed)
			{
				_connection.Open();
			}
		}

		public Int32 Add<TModel>(TModel model) where TModel : PropertyMonitor, new()
		{
			SqlBuilder<TModel> builder = new AddBuilder<TModel>(model, true);
			var entry = builder.Build();
			return SqlExecute($@"{entry.ToString()}", entry.ParameterMappers, CommandType.Text);
		}

		public Int32 Modify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
		{
			SqlBuilder<TModel> builder = new ModifyBuilder<TModel>(model, where, true);
			var entry = builder.Build();
			return SqlExecute(entry.ToString(), entry.ParameterMappers, CommandType.Text, true);
		}

		public List<TModel> Find<TModel>(Expression<Func<TModel, Boolean>> where, Expression<Func<TModel, Object>> fields) where TModel : PropertyMonitor, new()
		{
			var builder = new SelectBuilder<TModel>(null, where, fields);
			var entry = builder.Build();
			return Find<TModel>(entry.FormatSql(), entry.ParameterMappers, CommandType.Text);
		}

		public TModel FindOne<TModel>(Expression<Func<TModel, Boolean>> where, Expression<Func<TModel, Object>> fields) where TModel : PropertyMonitor, new()
		{
			var builder = new SelectBuilder<TModel>(null, where, fields);
			var entry = builder.Build();
			return Find<TModel>(entry.FormatSql(), entry.ParameterMappers, CommandType.Text).FirstOrDefault();
		}

		public TModel FindOne<TModel>(String sqlStr, IEnumerable<ParameterMapper> parameters = null, CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			return Find<TModel>(sqlStr, parameters, commandType).FirstOrDefault();
		}

		public List<TModel> Find<TModel>(String sqlStr, IEnumerable<ParameterMapper> parameters = null, CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			Open();
			using (DbCommand cmd = _connection.CreateCommand())
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

				var dr = cmd.ExecuteReader();
				var tmpDt = new DataTable("tmpDt");
				tmpDt.Load(dr, LoadOption.Upsert);
				dr.Close();
				return tmpDt.AsList<TModel>().ToList();
			}
		}

		public TValue FindSingleValue<TValue>(String sqlStr, IEnumerable<ParameterMapper> parameters = null, CommandType commandType = CommandType.Text)
		{
			Open();
			try
			{
				using (DbCommand cmd = _connection.CreateCommand())
				{
					if (_useTransaction)
					{
						cmd.Transaction = GetNonceTransaction();
					}
					cmd.CommandType = commandType;
					cmd.CommandText = sqlStr;
					//参数化
					if (parameters != null && parameters.Any())
					{
						cmd.Parameters.AddRange(parameters.Select(s => (DbParameter)s).ToArray());
					}

					TValue obj = (TValue)Convert.ChangeType(cmd.ExecuteScalar(), typeof(TValue));
					cmd.Parameters.Clear();
					return obj;
				}
			}
			catch (Exception)
			{
				throw;
			}
		}

		private Int32 SqlExecute(String sqlStr, IEnumerable<ParameterMapper> parameters = null, CommandType commandType = CommandType.Text, Boolean isModify = false)
		{
			Open();
			using (DbCommand cmd = _connection.CreateCommand())
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
				Int32 count = 0;
				if (!isModify)
				{
					count = Int32.Parse(cmd.ExecuteScalar().ToString());
				}
				else
				{
					count = Int32.Parse(cmd.ExecuteNonQuery().ToString());
				}
				cmd.Parameters.Clear();
				return count;
			}
		}

		#region 

		public void Dispose()
		{
			Dispose(true);
			GC.SuppressFinalize(this);
		}

		private void Dispose(Boolean disposing)
		{
			if (!disposed)
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
				disposed = true;
			}
		}

		#endregion
	}
}
