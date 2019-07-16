using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
    /// <summary>
    /// 翻译结果管理
    /// </summary>
    internal class TranslationResult
    {
        private StringBuilder _originSql;

        private IList<EntityParameter> _parameters;

        internal TranslationResult()
        {
            _originSql = new StringBuilder();
            _parameters = new List<EntityParameter>();
        }

        internal String GetSql()
        {
            return ReformatSql(_originSql.ToString());
        }

        internal IList<EntityParameter> GetParameters()
        {
            return _parameters;
        }

        internal String PrepareCacheKey()
        {
            Parameter.Validate(_originSql);
            var cacheKey = GetSql();
            foreach (var item in GetParameters())
            {
                cacheKey = cacheKey.Replace(item.Key, item.Value.ToString());
            }
            return MD.GetMD5(cacheKey);
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

        private String ReformatSql(String sql)
        {
            Parameter.Validate(sql);
            sql = sql.Replace("  ", " ");
            return sql;
        }

        public override String ToString()
        {
            return ReformatSql(_originSql.ToString());
        }
    }
}
