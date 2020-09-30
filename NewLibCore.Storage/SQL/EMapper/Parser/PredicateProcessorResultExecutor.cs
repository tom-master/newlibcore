﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    public sealed class PredicateProcessorResultExecutor : IDisposable
    {
        private readonly MapperDbContextBase _mapperDbContextBase;

        public PredicateProcessorResultExecutor(MapperDbContextBase mapperDbContextBase)
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
        internal SqlExecuteResultConvert Execute(PredicateProcessorResult processorResult)
        {
            var sql = processorResult.Sql.ToString();
            sql = ReformatSql(sql);
            var executeType = GetExecuteType(sql);
            var parameters = processorResult.Parameters.ToArray();
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
            if (sql[0] != ' ')
            {
                return sql;
            }
            sql = Regex.Replace(sql, "\\s{2,}", " ").Trim();
            return sql;
        }
    }
}
