﻿using System;
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
    internal sealed class ParserResult
    {
        private readonly StringBuilder _internalSql;

        private readonly QueryCacheBase _queryCacheBase;

        private readonly IList<MapperParameter> _parameters;

        private readonly MapperDbContextBase _mapperDbContextBase;

        /// <summary>
        /// 初始化一个ResultExecutor类的实例
        /// </summary>
        /// <param name="mapperDbContextBase"></param>
        /// <param name="queryCacheBase"></param>
        public ParserResult(MapperDbContextBase mapperDbContextBase, QueryCacheBase queryCacheBase)
        {
            Parameter.Validate(mapperDbContextBase);
            Parameter.Validate(queryCacheBase);

            _internalSql = new StringBuilder();
            _parameters = new List<MapperParameter>();

            _queryCacheBase = queryCacheBase;
            _mapperDbContextBase = mapperDbContextBase;
        }


        /// <summary>
        /// 追加一个sql语句
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="entityParameters"></param>
        /// <returns></returns>
        internal void Append(String sql)
        {
            Parameter.Validate(sql);
            _internalSql.Append($@" {sql} ");
        }

        /// <summary>
        /// 追加一组参数
        /// </summary>
        /// <param name="parameters"></param>
        internal void Append(params MapperParameter[] parameters)
        {
            Parameter.Validate(parameters);

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

            var executeResult = GetCache();
            if (executeResult == null)
            {
                var sql = ToString();

                var dbContext = _mapperDbContextBase;
                executeResult = dbContext.RawExecute(sql, _parameters.ToArray());
                var executeType = dbContext.GetExecuteType(sql);

                SetCache(executeType, executeResult);
            }

            return executeResult;
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
        /// 获取作为要缓存的sql语句的key
        /// </summary>
        /// <returns></returns>
        private String PrepareCacheKey()
        {
            Parameter.Validate(_internalSql);
            var cacheKey = ToString();
            foreach (var item in _parameters)
            {
                cacheKey = cacheKey.Replace(item.Key, item.Value.ToString());
            }
            return MD.GetMD5(cacheKey);
        }

        /// <summary>
        /// 设置缓存
        /// </summary>
        /// <param name="executeType"></param>
        /// <param name="executeResult"></param>
        private void SetCache(ExecuteType executeType, ExecuteResult executeResult)
        {
            Parameter.Validate(executeResult);
            if (executeType != ExecuteType.SELECT)
            {
                return;
            }

            if (_queryCacheBase != null)
            {
                _queryCacheBase.Add(PrepareCacheKey(), executeResult);
            }
        }

        /// <summary>
        /// 获取缓存
        /// </summary>
        /// <returns></returns>
        private ExecuteResult GetCache()
        {
            if (_queryCacheBase != null)
            {
                var cacheResult = _queryCacheBase.Get<ExecuteResult>(PrepareCacheKey());
                if (cacheResult != null)
                {
                    return cacheResult;
                }
            }
            return null;
        }

        /// <summary>
        /// 返回存储的sql语句
        /// </summary>
        /// <returns></returns>
        public override String ToString()
        {
            var sql = _internalSql.ToString();
            Parameter.Validate(sql);

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