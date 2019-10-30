using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;
using Microsoft.Extensions.DependencyInjection;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 存储表达式的翻译后的sql语句
    /// </summary>
    internal sealed class TranslateResult
    {
        private StringBuilder _originSql;

        private readonly IList<EntityParameter> _parameters;

        private readonly ResultCache _cache = new ExecutionResultCache();

        /// <summary>
        /// 初始化一个TranslationResult类的实例
        /// </summary>
        private TranslateResult()
        {
            _originSql = new StringBuilder();
            _parameters = new List<EntityParameter>();

        }

        /// <summary>
        /// 创建一个TranslationResult类的实例
        /// </summary>
        /// <returns></returns>
        internal static TranslateResult CreateResult()
        {
            return new TranslateResult();
        }

        /// <summary>
        /// 追加一个sql语句和一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters">参数列表</param>
        internal TranslateResult Append(String sql, IEnumerable<EntityParameter> entityParameters = null)
        {
            Parameter.Validate(sql);

            _originSql.Append($@" {sql} ");
            if (entityParameters != null)
            {
                foreach (var item in entityParameters)
                {
                    _parameters.Add(item);
                }
            }

            return this;
        }

        /// <summary>
        /// 追加一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters">参数列表</param>
        internal void Append(params EntityParameter[] entityParameters)
        {
            Append(entityParameters.ToList());
        }

        /// <summary>
        /// 追加一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters">参数列表</param>
        internal void Append(IEnumerable<EntityParameter> entityParameters)
        {
            if (entityParameters != null)
            {
                foreach (var item in entityParameters)
                {
                    _parameters.Add(item);
                }
            }
        }

        /// <summary>
        /// 执行表达式翻译出的sql语句
        /// </summary>
        /// <param name="executionCore">执行翻译结果的对象</param>
        /// <returns></returns>
        internal RawResult Execute()
        {
            var dbContext = _serviceProvider.GetService<IMapperDbContext>();

            Console.WriteLine(dbContext.GetHashCode());
            var executeResult = GetCache();
            var executeType = dbContext.GetExecuteType(ToString());
            if (executeResult == null)
            {
                executeResult = dbContext.RawExecute(ToString(), _parameters);
                SetCache(executeType, executeResult);
            }

            return executeResult;
        }

        /// <summary>
        /// 清空上次使用后留下的数据
        /// </summary>
        internal void Clear()
        {
            _originSql.Clear();
            _parameters.Clear();
        }

        /// <summary>
        /// 获取作为要缓存的sql语句的key
        /// </summary>
        /// <returns></returns>
        private String PrepareCacheKey()
        {
            Parameter.Validate(_originSql);
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
        /// <param name="executeResult">sql执行后原始的执行结果</param>
        private void SetCache(ExecuteType executeType, RawResult executeResult)
        {
            if (_cache != null)
            {
                if (executeType == ExecuteType.SELECT)
                {
                    _cache.Add(PrepareCacheKey(), executeResult);
                }
            }
        }

        /// <summary>
        /// 获取缓存
        /// </summary>
        /// <returns></returns>
        private RawResult GetCache()
        {
            if (_cache != null)
            {
                var cacheResult = _cache.Get(PrepareCacheKey());
                if (cacheResult != null)
                {
                    return (RawResult)cacheResult;
                }
            }

            return default;
        }

        /// <summary>
        /// 返回存储的sql语句
        /// </summary>
        /// <returns></returns>
        public override String ToString()
        {
            Parameter.Validate(_originSql);
            _originSql = _originSql.Replace("   ", " ").Replace("  ", " ");
            return _originSql.ToString().Trim();
        }
    }
}
