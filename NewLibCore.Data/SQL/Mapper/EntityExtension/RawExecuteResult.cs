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
    internal sealed class RawExecuteResult
    {
        private Object _result;

        /// <summary>
        /// 初始化一个RawExecuteResult类的实例
        /// </summary>
        internal RawExecuteResult()
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
        /// 返回.net的原生类型
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        internal TResult ToPrimitive<TResult>()
        {
            var modelType = typeof(TResult);
            if (modelType.IsComplexType())
            {
                throw new InvalidCastException($@"当返回.net的原生类型时需要调用ToPrimitive方法");
            }
            return (TResult)Convert.ChangeType(_result, typeof(TResult));
        }

        /// <summary>
        /// 返回继承于EntityBase的集合对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        internal List<TResult> ToList<TResult>() where TResult : new()
        {
            return ((DataTable)_result).ToList<TResult>();
        }

        /// <summary>
        /// 返回继承于EntityBase的单个对象
        /// </summary>
        /// <typeparam name="TResult">原生类型</typeparam>
        /// <returns></returns>
        internal TResult ToSingle<TResult>() where TResult : new()
        {
            return ToList<TResult>().FirstOrDefault();
        }

        public override String ToString()
        {
            return JsonConvert.SerializeObject(_result);
        }
    }
}
