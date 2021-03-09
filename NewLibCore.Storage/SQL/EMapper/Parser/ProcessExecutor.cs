using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    public sealed class ProcessExecutor : IDisposable
    {
        private readonly StringBuilder _innerSql;

        private readonly IList<MapperParameter> _parameters;

        private readonly MapperDbContextBase _mapperDbContextBase;

        /// <summary>
        /// 初始化一个ResultExecutor类的实例
        /// </summary>
        /// <param name="mapperDbContextBase"></param>
        public ProcessExecutor(MapperDbContextBase mapperDbContextBase)
        {
            Check.IfNullOrZero(mapperDbContextBase);

            _innerSql = new StringBuilder();
            _parameters = new List<MapperParameter>();

            _mapperDbContextBase = mapperDbContextBase;
        }

        /// <summary>
        /// 追加一组参数
        /// </summary>
        /// <param name="parameters"></param>
        internal void Append(params MapperParameter[] parameters)
        {
            Check.IfNullOrZero(parameters);

            foreach (var item in parameters)
            {
                _parameters.Add(item);
            }
        }

        internal void Append(String sql, params MapperParameter[] parameters)
        {
            _innerSql.Append($@" {sql} ");
            Append(parameters);
        }

        /// <summary>
        /// 执行表达式翻译出的sql语句
        /// </summary>
        /// <returns></returns>
        internal SqlExecuteResultConvert Execute()
        {
            var sql = ToString();
            var executeType = GetExecuteType(sql);

            switch (executeType)
            {
                case ExecuteType.SELECT:
                    return _mapperDbContextBase.Select(sql, _parameters.ToArray());
                case ExecuteType.UPDATE:
                    return _mapperDbContextBase.Update(sql, _parameters.ToArray());
                case ExecuteType.INSERT:
                    return _mapperDbContextBase.Insert(sql, _parameters.ToArray());
                default:
                    throw new InvalidOperationException($@"无效的sql语句操作{executeType}");
            }
        }

        private ExecuteType GetExecuteType(String sql)
        {
            Check.IfNullOrZero(sql);

            var operationType = sql.Substring(0, sql.IndexOf(" "));
            if (Enum.TryParse<ExecuteType>(operationType, out var executeType))
            {
                return executeType;
            }

            throw new Exception($@"SQL语句执行类型解析失败:{operationType}");
        }

        /// <summary>
        /// 返回存储的sql语句
        /// </summary>
        /// <returns></returns>
        public override String ToString()
        {
            var sql = _innerSql.ToString();
            Check.IfNullOrZero(sql);

            if (sql[0] != ' ')
            {
                return sql;
            }
            _innerSql.Clear();

            sql = Regex.Replace(sql, "\\s{2,}", " ").Trim();
            _innerSql.Append(sql);
            return sql;
        }

        public void ClearSql()
        {
            _innerSql.Clear();
        }

        public void ClearParameter()
        {
            _parameters.Clear();
        }

        public void Dispose()
        {
            ClearSql();
            ClearParameter();
        }
    }
}
