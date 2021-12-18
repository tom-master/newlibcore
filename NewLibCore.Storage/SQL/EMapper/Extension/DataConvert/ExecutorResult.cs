﻿using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

using NewLibCore.Storage.SQL.DataConvert;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// sql语句执行后的结果
    /// </summary>
    public sealed class ExecutorResult
    {
        private Object _result;

        /// <summary>
        /// 保存语句执行后的原始结果
        /// </summary>
        /// <param name="rawResult">语句执行后的原始结果</param>
        internal void SaveRawResult(Object rawResult)
        {
            Check.IfNullOrZero(rawResult);
            _result = rawResult;
        }

        internal Int32 GetModifyRowCount()
        {
            if (_result is DataTable table)
            {
                return table.Rows[0][0].CastTo<Int32>();
            }
            return _result.CastTo<Int32>();
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
