using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Translation
{
    /// <summary>
    /// 存储表达式的翻译结果
    /// </summary>
    internal class TranslateResult
    {
        private readonly StringBuilder _originSql;

        private readonly IList<EntityParameter> _parameters;

        internal TranslateResult()
        {
            _originSql = new StringBuilder();
            _parameters = new List<EntityParameter>();
        }

        internal ExecuteType ExecuteType { get; set; }

        /// <summary>
        /// 获取翻译好的sql语句
        /// </summary>
        /// <returns></returns>
        internal String GetSql()
        {
            return ReformatSql(_originSql.ToString());
        }

        /// <summary>
        /// 获取EntityParameter列表
        /// </summary>
        /// <returns></returns>
        internal IList<EntityParameter> GetParameters()
        {
            return _parameters;
        }

        /// <summary>
        /// 获取作为要缓存的sql语句的key
        /// </summary>
        /// <param name="entityParameters"></param>
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

        /// <summary>
        /// 追加一个sql语句和一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters"></param>
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

        /// <summary>
        /// 追加一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters"></param>
        internal void Append(params EntityParameter[] entityParameters)
        {
            Append(entityParameters.ToList());
        }

        /// <summary>
        /// 追加一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters"></param>
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

        /// <summary>
        /// 将sql语句中多余的空格去掉
        /// </summary>
        /// <param name="sql"></param>
        /// <returns></returns>
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
