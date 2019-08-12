using System;
using System.Collections.Generic;
using System.Data;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    /// <summary>
    /// 存储未包装过的sql执行结果
    /// </summary>
    internal class RawExecuteResult
    {
        private Object _result;

        internal RawExecuteResult()
        {
        }

        internal void SetRawResult(Object rawResult)
        {
            Parameter.Validate(rawResult);
            _result = rawResult;
        }

        /// <summary>
        /// 返回.net的原生类型
        /// </summary>
        /// <typeparam name="TResult"></typeparam>
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
        /// <typeparam name="TResult"></typeparam>
        /// <returns></returns>
        internal List<TResult> ToList<TResult>() where TResult : new()
        {
            var modelType = typeof(TResult);
            if (!modelType.IsComplexType())
            {
                throw new InvalidCastException($@"当返回继承于EntityBase的集合对象时需要调用ToList方法");
            }
            return ((DataTable)_result).ToList<TResult>();
        }

        /// <summary>
        /// 返回继承于EntityBase的单个对象
        /// </summary>
        /// <typeparam name="TResult"></typeparam>
        /// <returns></returns>
        internal TResult ToSingle<TResult>() where TResult : new()
        {
            var modelType = typeof(TResult);
            if (!modelType.IsComplexType())
            {
                throw new InvalidCastException($@"当返回继承于EntityBase的单个对象时需要调用ToSingle方法");
            }
            return ((DataTable)_result).ToSingle<TResult>();
        }
    }
}
