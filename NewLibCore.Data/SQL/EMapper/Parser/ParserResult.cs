using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using NewLibCore.Data.SQL.Component.Cache;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL
{

    /// <summary>
    /// 执行Expression翻译后的sql语句
    /// </summary>
    internal sealed class ExpressionProcessorResult
    {
        private readonly StringBuilder _internalSql;

        private readonly IList<MapperParameter> _parameters;

        private readonly MapperDbContextBase _mapperDbContextBase;

        /// <summary>
        /// 初始化一个ResultExecutor类的实例
        /// </summary>
        /// <param name="mapperDbContextBase"></param>
        /// <param name="queryCacheBase"></param>
        public ExpressionProcessorResult(MapperDbContextBase mapperDbContextBase)
        {
            Parameter.IfNullOrZero(mapperDbContextBase);

            _internalSql = new StringBuilder();
            _parameters = new List<MapperParameter>();

            _mapperDbContextBase = mapperDbContextBase;
        }


        /// <summary>
        /// 追加一个sql语句
        /// </summary>
        /// <param name="sql"></param>
        internal void Append(String sql)
        {
            Parameter.IfNullOrZero(sql);
            _internalSql.Append($@" {sql} ");
        }

        /// <summary>
        /// 追加一组参数
        /// </summary>
        /// <param name="parameters"></param>
        internal void Append(params MapperParameter[] parameters)
        {
            Parameter.IfNullOrZero(parameters);

            foreach (var item in parameters)
            {
                _parameters.Add(item);
            }
        }

        internal void Append(String sql, params MapperParameter[] parameters)
        {
            Append(sql);
            Append(parameters);
        }

        /// <summary>
        /// 执行表达式翻译出的sql语句
        /// </summary>
        /// <returns></returns>
        internal ExecuteResult Execute()
        {
            var sql = ToString();
            var executeType = GetExecuteType(sql);
            return _mapperDbContextBase.RawExecute(executeType, sql, _parameters.ToArray());
        }

        private ExecuteType GetExecuteType(String sql)
        {
            Parameter.IfNullOrZero(sql);

            var operationType = sql.Substring(0, sql.IndexOf(" "));
            if (Enum.TryParse<ExecuteType>(operationType, out var executeType))
            {
                return executeType;
            }

            throw new Exception($@"SQL语句执行类型解析失败:{operationType}");
        }

        /// <summary>
        /// 清空上次使用后留下的语句
        /// </summary>
        internal void ClearSql()
        {
            _internalSql.Clear();
        }

        /// <summary>
        /// 清空上次使用后留下的参数
        /// </summary>
        internal void ClearParameter()
        {
            _parameters.Clear();
        }


        /// <summary>
        /// 返回存储的sql语句
        /// </summary>
        /// <returns></returns>
        public override String ToString()
        {
            var sql = _internalSql.ToString();
            Parameter.IfNullOrZero(sql);

            if (sql[0] != ' ')
            {
                return sql;
            }
            _internalSql.Clear();

            sql = Regex.Replace(sql, "\\s{2,}", " ").Trim();
            _internalSql.Append(sql);
            return sql;
        }
    }
}
