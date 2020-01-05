using System;
using System.Collections.Generic;
using System.Data;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;
using Newtonsoft.Json;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// sql语句执行后的结果
    /// </summary>
    public sealed class ExecuteResult
    {
        private Object _result;

        /// <summary>
        /// 初始化一个RawExecuteResult类的实例
        /// </summary>
        internal ExecuteResult()
        {
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
            try
            {
                return ((DataTable)_result).ToList<TResult>();
            }
            catch (System.Exception)
            {
                throw;
            }
        }

        /// <summary>
        /// 返回单个对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        public TResult FirstOrDefault<TResult>()
        {
            try
            {
                if (!typeof(TResult).IsComplexType() && !_result.GetType().IsComplexType())
                {
                    return (TResult)Convert.ChangeType(_result, typeof(TResult));
                }
                return ((DataTable)_result).FirstOrDefault<TResult>();
            }
            catch (Exception)
            {
                throw;
            }
        }

        public override String ToString()
        {
            return JsonConvert.SerializeObject(_result);
        }
    }
}
