﻿using System;
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
	public class DataStore: IDisposable
	{
		private MySqlConnection _connection;
		private MySqlTransaction _dataTransaction;
		private Boolean disposed = false;

		public DataStore(String connection)
		{
			_connection = new MySqlConnection(connection);
		}

		#region 事务处理

		public void OpenTransaction()
		{
			UseTransaction = true;
		}

		private Boolean UseTransaction { get; set; }

		private MySqlTransaction GetNonceTransaction()
		{
			if (UseTransaction)
			{
				if (_dataTransaction == null)
				{
					UseTransaction = true;
					_dataTransaction = _connection.BeginTransaction();
				}
				return _dataTransaction;
			}
			throw new Exception("没有启动事务");
		}

		public void Commit()
		{
			if (UseTransaction)
			{
				_dataTransaction.Commit();
			}
			else
			{
				throw new Exception("没有启动事务，无法执行事务提交");
			}
		}

		public void Rollback()
		{
			if (UseTransaction)
			{
				_dataTransaction?.Rollback();
			}
			else
			{
				throw new Exception("没有启动事务，无法执行事务回滚");
			}
		}

		#endregion

		public Int32 ExecuteAdd<TModel>(TModel model) where TModel : class, new()
		{
			SqlBuilder<TModel> builder = new InsertBuilder<TModel>(model, true);
			var entry = builder.Build();
			return SqlExecute(entry.Sql, entry.Parameters);
		}

		public Int32 ExecuteModify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where = default(Expression<Func<TModel, Boolean>>)) where TModel : PropertyMonitor, new()
		{
			SqlBuilder<TModel> builder = new UpdateBuilder<TModel>(model, where);
			var entry = builder.Build();
			return SqlExecute(entry.Sql, entry.Parameters);
		}

		private Int32 SqlExecute(String sqlStr, IEnumerable<ParameterMapper> parameters = default(IEnumerable<ParameterMapper>), CommandType commandType = CommandType.Text)
		{
			Open();
			using (MySqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
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

		public TValue FindSingleValue<TValue>(String sqlStr, IEnumerable<ParameterMapper> parameters = default(IEnumerable<ParameterMapper>), CommandType commandType = CommandType.Text)
		{
			Open();
			try
			{
				using (MySqlCommand cmd = _connection.CreateCommand())
				{
					if (UseTransaction)
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

		public List<TModel> Find<TModel>(String sqlStr, IEnumerable<ParameterMapper> parameters = default(IEnumerable<ParameterMapper>), CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			Open();
			using (MySqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
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

		public TModel FindOne<TModel>(String sqlStr, IEnumerable<ParameterMapper> parameters = default(IEnumerable<ParameterMapper>), CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			return Find<TModel>(sqlStr, parameters, commandType).FirstOrDefault();
		}

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

		private void Open()
		{
			if (_connection.State == ConnectionState.Closed)
			{
				_connection.Open();
			}
		}

	}
}
