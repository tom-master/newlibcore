using System;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 谓词表达式翻译结果执行
    /// </summary>
    public sealed class ResultExecutor : IDisposable
    {
        private readonly MapperDbContextBase _mapperDbContextBase;

        public ResultExecutor(MapperDbContextBase mapperDbContextBase)
        {
            _mapperDbContextBase = mapperDbContextBase;
        }

        public void Dispose()
        {
            _mapperDbContextBase.Dispose();
        }

        /// <summary>
        /// 执行表达式翻译出的sql语句
        /// </summary>
        /// <returns></returns>
        internal ExecutorResult Execute(StatementResultBuilder statementResultBuilder)
        {
            var sql = statementResultBuilder.Build();
            sql = ReformatSql(sql);
            var executeType = GetExecuteType(sql);
            var parameters = statementResultBuilder.Parameters.ToArray();
            statementResultBuilder.Clear();
            switch (executeType)
            {
                case ExecuteType.SELECT:
                    return _mapperDbContextBase.Select(sql, parameters);
                case ExecuteType.UPDATE:
                    return _mapperDbContextBase.Update(sql, parameters);
                case ExecuteType.INSERT:
                    return _mapperDbContextBase.Insert(sql, parameters);
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


        private String ReformatSql(string sql)
        {
            Check.IfNullOrZero(sql);
            // if (sql[0] != ' ')
            // {
            //     return sql;
            // }
            sql = Regex.Replace(sql, "\\s{2,}", " ").Trim();
            return sql;
        }
    }
}
