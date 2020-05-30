using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using NewLibCore.Data.SQL.Component.Cache;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Validate;
using Newtonsoft.Json;

namespace NewLibCore.Data.SQL
{
    /// <summary>
    /// sql语句执行后的结果
    /// </summary>
    public sealed class ExecuteResult
    {
        private Object _result;
        private readonly QueryCacheBase _queryCacheBase;

        /// <summary>
        /// 初始化一个RawExecuteResult类的实例
        /// </summary>
        internal ExecuteResult(QueryCacheBase queryCacheBase)
        {
            _queryCacheBase = queryCacheBase;
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

        /// <summary>
        /// 返回集合对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        public List<TResult> ToList<TResult>() where TResult : new()
        {
            var key = ToString();
            var cache = GetCache<List<TResult>>(key);
            if (cache != null)
            {
                return cache;
            }

            var result = ((DataTable)_result).ToList<TResult>();
            SetCache(key, result);
            return result;
        }

        /// <summary>
        /// 返回单个对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        public TResult FirstOrDefault<TResult>()
        {
            var complexType = typeof(TResult).IsComplexType();
            if (!complexType && !_result.GetType().IsComplexType())
            {
                return (TResult)Convert.ChangeType(_result, typeof(TResult));
            }

            var key = ToString();
            if (complexType)
            {
                var cache = GetCache<TResult>(key);
                if (cache != null)
                {
                    return cache;
                }
            }

            var result = ((DataTable)_result).ToList<TResult>().FirstOrDefault();
            if (complexType)
            {
                SetCache(key, result);
            }
            return result;
        }

        private T GetCache<T>(String key)
        {
            Parameter.Validate(key);

            var result = _queryCacheBase.Get<T>(key);
            if (result != null)
            {
                return result;
            }
            return default(T);
        }

        private void SetCache(String key, Object value)
        {
            Parameter.Validate(key);
            Parameter.Validate(value);

            _queryCacheBase.Add(key, value);
        }

        public override String ToString()
        {
            return JsonConvert.SerializeObject(_result);
        }
    }
}
