﻿using System;
using System.Linq;
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
            using (statementResultBuilder)
            {
                var (sql, parameters) = statementResultBuilder.Build();
                var parametersInternal = parameters.ToArray();
                sql = ReformatSql(sql);
                var executeType = GetExecuteType(sql);
                switch (executeType)
                {
                    case ExecuteType.SELECT:
                        return _mapperDbContextBase.Select(sql, parametersInternal);
                    case ExecuteType.UPDATE:
                        return _mapperDbContextBase.Update(sql, parametersInternal);
                    case ExecuteType.INSERT:
                        return _mapperDbContextBase.Insert(sql, parametersInternal);
                    default:
                        throw new InvalidOperationException($@"无效的sql语句操作{executeType}");
                }
            }
        }

        private ExecuteType GetExecuteType(string sql)
        {
            Check.IfNullOrZero(sql);

            var operationType = sql.Substring(0, sql.IndexOf(" "));
            if (Enum.TryParse<ExecuteType>(operationType, out var executeType))
            {
                return executeType;
            }

            throw new Exception($@"SQL语句执行类型解析失败:{operationType}");
        }

        private string ReformatSql(string sql)
        {
            Check.IfNullOrZero(sql);
            sql = Regex.Replace(sql, "\\s{2,}", " ").Trim();
            return sql;
        }
    }
}
