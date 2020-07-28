﻿using System;
using System.Linq;
using System.Data;
using System.Collections.Generic;
using NewLibCore.Validate;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.Component.Cache;

namespace NewLibCore.Data.SQL
{
    /// <summary>
    /// sql语句执行后的结果
    /// </summary>
    public sealed class ExecuteResult
    {
        private Object _result;
        // private readonly QueryCacheBase _queryCacheBase;

        /// <summary>
        /// 初始化一个RawExecuteResult类的实例
        /// </summary>
        internal ExecuteResult(QueryCacheBase _)
        {
            // _queryCacheBase = queryCacheBase;
        }

        /// <summary>
        /// 保存语句执行后的原始结果
        /// </summary>
        /// <param name="rawResult">语句执行后的原始结果</param>
        internal void SaveRawResult(Object rawResult)
        {
            Parameter.Validate(rawResult);
            _result = rawResult;
        }

        internal Int32 GetModifyRowCount()
        {
            if (_result is DataTable table)
            {
                return (Int32)(table.Rows[0][0]).ChangeType(typeof(Int32));
            }
            return (Int32)_result.ChangeType(typeof(Int32));
        }

        /// <summary>
        /// 返回集合对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        public List<TResult> ToList<TResult>() where TResult : new()
        {
            var result = ((DataTable)_result).ToList<TResult>();
            return result;
        }

        /// <summary>
        /// 返回单个对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        public TResult FirstOrDefault<TResult>() where TResult : new()
        {
            var result = ((DataTable)_result).ToList<TResult>().FirstOrDefault();
            return result;
        }
    }
}