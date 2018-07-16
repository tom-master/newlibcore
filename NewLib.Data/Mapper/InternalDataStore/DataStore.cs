using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Linq.Expressions;
using NewLib.Data.Mapper.DataExtension;
using NewLib.Security;

namespace NewLib.Data.Mapper.InternalDataStore
{
	public class DataStore: IDisposable
	{
		private SqlConnection _connection;
		private SqlTransaction _dataTransaction;
		private Boolean disposed = false;

		public DataStore(String connectionName = default(String))
		{
			var conn = SensitiveDataSafetyProvider.Decrypt(ConfigurationManager.ConnectionStrings[connectionName ?? "NewCrm"].ToString());
			_connection = new SqlConnection(conn);
		}

		#region 事务处理

		public void OpenTransaction()
		{
			UseTransaction = true;
		}

		/// <summary>
		/// 是否使用事务
		/// </summary>
		private Boolean UseTransaction { get; set; }

		/// <summary>
		/// 获得当前事务
		/// </summary>
		/// <returns></returns>
		protected SqlTransaction GetNonceTransaction()
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

		/// <summary>
		/// 事务提交
		/// </summary>
		public virtual void Commit()
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

		/// <summary>
		/// 事务回滚
		/// </summary>
		public virtual void Rollback()
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
			var sql = builder.ParseToSql();
			var parameters = builder.GetParameters();

			return SqlExecute(sql, parameters);
		}

		public Int32 ExecuteModify<TModel>(TModel model, Expression<Func<TModel, Boolean>> where) where TModel : class, new()
		{
			SqlBuilder<TModel> builder = new UpdateBuilder<TModel>(model, where);
			var sql = builder.ParseToSql();
			var parameters = builder.GetParameters();

			return SqlExecute(sql, parameters);
		}

		private Int32 SqlExecute(String sqlStr, CommandType commandType = CommandType.Text)
		{
			Open();
			using (SqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
				{
					cmd.Transaction = GetNonceTransaction();
				}
				cmd.CommandType = commandType;
				cmd.CommandText = sqlStr;
				return cmd.ExecuteNonQuery();
			}
		}

		private Int32 SqlExecute(String sqlStr, IEnumerable<SqlParameter> parameters, CommandType commandType = CommandType.Text)
		{
			Open();
			using (SqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
				{
					cmd.Transaction = GetNonceTransaction();
				}
				cmd.CommandType = commandType;
				cmd.CommandText = sqlStr;
				if (parameters.Any())
				{
					cmd.Parameters.AddRange(parameters.ToArray());
				}
				Int32 count = cmd.ExecuteNonQuery();
				cmd.Parameters.Clear();
				return count;
			}
		}

		public List<TModel> Find<TModel>(String sqlStr, CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			Open();
			using (SqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
				{
					cmd.Transaction = GetNonceTransaction();
				}
				cmd.CommandType = commandType;
				cmd.CommandText = sqlStr;
				SqlDataReader dr = cmd.ExecuteReader();
				DataTable dataTable = new DataTable("tmpDt");
				dataTable.Load(dr, LoadOption.Upsert);
				var result = dataTable.AsList<TModel>().ToList();
				return result;
			}
		}

		public List<TModel> Find<TModel>(String sqlStr, IEnumerable<SqlParameter> parameters, CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			Open();
			using (SqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
				{
					cmd.Transaction = GetNonceTransaction();
				}
				cmd.CommandType = commandType;
				cmd.CommandText = sqlStr;
				if (parameters.Any())
				{
					cmd.Parameters.AddRange(parameters.ToArray());
				}

				IDataReader dr = cmd.ExecuteReader();
				var tmpDt = new DataTable("tmpDt");
				tmpDt.Load(dr, LoadOption.Upsert);
				return tmpDt.AsList<TModel>().ToList();
			}
		}

		public TValue FindSingleValue<TValue>(String sqlStr, CommandType commandType = CommandType.Text)
		{
			Open();
			try
			{
				using (SqlCommand cmd = _connection.CreateCommand())
				{
					if (UseTransaction)
					{
						cmd.Transaction = GetNonceTransaction();
					}
					cmd.CommandType = commandType;
					cmd.CommandText = sqlStr;
					TValue obj = (TValue)cmd.ExecuteScalar();
					cmd.Parameters.Clear();
					return obj;
				}
			}
			catch (Exception)
			{
				throw;
			}
		}

		public TValue FindSingleValue<TValue>(String sqlStr, IEnumerable<SqlParameter> parameters, CommandType commandType = CommandType.Text)
		{
			Open();
			try
			{
				using (SqlCommand cmd = _connection.CreateCommand())
				{
					if (UseTransaction)
					{
						cmd.Transaction = GetNonceTransaction();
					}
					cmd.CommandType = commandType;
					cmd.CommandText = sqlStr;
					//参数化
					if (parameters.Any())
					{
						cmd.Parameters.AddRange(parameters.ToArray());
					}

					TValue obj = (TValue)cmd.ExecuteScalar();
					cmd.Parameters.Clear();
					return obj;
				}
			}
			catch (Exception)
			{
				throw;
			}
		}

		public TModel FindOne<TModel>(String sqlStr, CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			return Find<TModel>(sqlStr, commandType).FirstOrDefault();
		}

		public TModel FindOne<TModel>(String sqlStr, IEnumerable<SqlParameter> parameters, CommandType commandType = CommandType.Text) where TModel : class, new()
		{
			return Find<TModel>(sqlStr, parameters, commandType).FirstOrDefault();
		}

		public SqlDataReader SqlGetDataReader(String sqlStr, IEnumerable<SqlParameter> parameters, CommandType commandType = CommandType.Text)
		{
			Open();
			using (SqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
				{
					cmd.Transaction = GetNonceTransaction();
				}
				cmd.CommandType = commandType;
				cmd.CommandText = sqlStr;
				//参数化
				if (parameters.Any())
				{
					cmd.Parameters.AddRange(parameters.ToArray());
				}
				var dr = cmd.ExecuteReader();
				cmd.Parameters.Clear();
				return dr;
			}
		}

		public SqlDataReader SqlGetDataReader(String sqlStr, CommandType commandType = CommandType.Text)
		{
			Open();
			using (SqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
				{
					cmd.Transaction = GetNonceTransaction();
				}
				cmd.CommandType = commandType;
				cmd.CommandText = sqlStr;
				return cmd.ExecuteReader();
			}
		}

		public SqlDataReader SqlGetDataReader(String sqlStr, CommandBehavior behavior, CommandType commandType = CommandType.Text)
		{
			Open();
			using (SqlCommand cmd = _connection.CreateCommand())
			{
				if (UseTransaction)
				{
					cmd.Transaction = GetNonceTransaction();
				}
				cmd.CommandType = commandType;
				cmd.CommandText = sqlStr;
				return cmd.ExecuteReader(behavior);
			}
		}

		public void Dispose()
		{
			Dispose(true);
			GC.SuppressFinalize(this);
		}

		protected virtual void Dispose(Boolean disposing)
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
