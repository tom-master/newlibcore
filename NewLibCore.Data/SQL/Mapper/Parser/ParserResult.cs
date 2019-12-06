using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Component.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 存储表达式的翻译后的sql语句
    /// </summary>
    internal sealed class ParserResult
    {
        private StringBuilder _originSql;

        private readonly IList<MapperParameter> _parameters;

        private readonly QueryCacheBase _queryCache = MapperConfig.QueryCache;

        /// <summary>
        /// 初始化一个TranslationResult类的实例
        /// </summary>
        private ParserResult()
        {
            _originSql = new StringBuilder();
            _parameters = new List<MapperParameter>();
        }

        /// <summary>
        /// 创建一个TranslationResult类的实例
        /// </summary>
        /// <returns></returns>
        internal static ParserResult CreateResult()
        {
            return new ParserResult();
        }

        /// <summary>
        /// 追加一个sql语句和一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters">参数列表</param>
        internal ParserResult Append(String sql, IEnumerable<MapperParameter> entityParameters = null)
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
        /// 执行表达式翻译出的sql语句
        /// </summary>
        /// <param name="executionCore">执行翻译结果的对象</param>
        /// <returns></returns>
        internal ExecuteResult Execute(IServiceProvider serviceProvider)
        {
            var dbContext = serviceProvider.GetService<MapperDbContextBase>();
            var executeType = dbContext.GetExecuteType(ToString());

            var executeResult = GetCache();
            if (executeResult == null)
            {
                executeResult = dbContext.RawExecute(ToString(), _parameters);
                SetCache(executeType, executeResult);
            }

            return executeResult;
        }

        /// <summary>
        /// 清空上次使用后留下的语句
        /// </summary>
        internal void ClearSql()
        {
            _originSql.Clear();
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
        /// <param name="executeType"></param>
        /// <param name="executeResult"></param>
        private void SetCache(ExecuteType executeType, ExecuteResult executeResult)
        {
            if (executeType != ExecuteType.SELECT)
            {
                return;
            }

            if (_queryCache != null)
            {
                _queryCache.Add(PrepareCacheKey(), executeResult);
            }
        }

        /// <summary>
        /// 获取缓存
        /// </summary>
        /// <returns></returns>
        private ExecuteResult GetCache()
        {
            if (_queryCache != null)
            {
                var cacheResult = _queryCache.Get(PrepareCacheKey());
                if (cacheResult != null)
                {
                    RunDiagnosis.Info($@"SQL查询缓存返回:{cacheResult}");
                    return (ExecuteResult)cacheResult;
                }
            }
            RunDiagnosis.Warn($@"SQL查询缓存为NULL,将进行实际的SQL查询");
            return null;
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
