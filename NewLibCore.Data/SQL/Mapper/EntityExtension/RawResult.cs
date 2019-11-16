using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using NewLibCore.Validate;
using Newtonsoft.Json;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    /// <summary>
    /// 存储未包装过的sql执行结果
    /// </summary>
    public sealed class RawResult
    {
        private Object _result;

        /// <summary>
        /// 初始化一个RawExecuteResult类的实例
        /// </summary>
        internal RawResult()
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
            return ((DataTable)_result).ToList<TResult>();
        }

        /// <summary>
        /// 返回单个对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        public TResult FirstOrDefault<TResult>() where TResult : new()
        {
            try
            {
                if (!typeof(TResult).IsComplexType())
                {
                    return (TResult)_result;
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
