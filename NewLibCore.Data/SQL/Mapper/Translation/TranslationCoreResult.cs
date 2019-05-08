using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
	internal class TranslationCoreResult
	{
		private StringBuilder _originSql;

		private IList<EntityParameter> _parameters;

		internal TranslationCoreResult()
		{
			_originSql = new StringBuilder();
			_parameters = new List<EntityParameter>();
		}

		internal String GetSql()
		{
			return _originSql.ToString();
		}

		internal IList<EntityParameter> GetParameters()
		{
			return _parameters;
		}

		internal void Append(String sql, IEnumerable<EntityParameter> entityParameters = null)
		{
			Parameter.Validate(sql);

			_originSql.Append($@" {sql} ");
			if (entityParameters != null)
			{
				foreach (var item in entityParameters)
				{
					_parameters.Add(item);
				}
			}
		}

		internal void Append(params EntityParameter[] entityParameters)
		{
			Append(entityParameters.ToList());
		}

		internal void Append(IEnumerable<EntityParameter> entityParameters)
		{
			if (entityParameters != null)
			{
				foreach (var item in entityParameters)
				{
					_parameters.Add(item);
				}
			}
		}

		internal void Clear()
		{
			_originSql.Clear();
			_parameters.Clear();
		}
	}
}
