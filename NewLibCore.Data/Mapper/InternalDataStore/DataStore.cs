using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Data.SqlClient;
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

		public DataStore(String connection)
		{
			_connection = new MySqlConnection(connection);
		}

		#region 事务处理

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

		#endregion

		public Int32 ExecuteAdd<TModel>(TModel model) where TModel : class, new()
		{
			SqlBuilder<TModel> builder = new AddBuilder<TModel>(model, true);
			var entry = builder.Build();
			return SqlExecute(entry.ToString(), entry.Parameters);
		}

		public Int32 ExecuteModify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = null) where TModel : PropertyMonitor, new()
		{
			SqlBuilder<TModel> builder = new ModifyBuilder<TModel>(model, where, true);
			var entry = builder.Build();
			return SqlExecute(entry.ToString(), entry.Parameters);
		}

		private Int32 SqlExecute(String sqlStr, IEnumerable<ParameterMapper> parameters = null, CommandType commandType = CommandType.Text)
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
				Int32 count = cmd.ExecuteNonQuery();
				cmd.Parameters.Clear();
				return count;
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

		public TModel FindOne<TModel>(String sqlStr, IEnumerable<ParameterMapper> parameters = null, CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			return Find<TModel>(sqlStr, parameters, commandType).FirstOrDefault();
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
					return;

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

		private void Open()
		{
			if (_connection.State == ConnectionState.Closed)
			{
				_connection.Open();
			}
		}

	}
}
