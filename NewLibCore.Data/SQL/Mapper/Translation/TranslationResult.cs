using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 存储表达式的翻译后的sql语句
    /// </summary>
    internal sealed class TranslationResult
    {
        private readonly StringBuilder _originSql;
        private readonly IList<EntityParameter> _parameters;
        private readonly ResultCache _cache = null;

        /// <summary>
        /// 初始化一个TranslationResult类的实例
        /// </summary>
        private TranslationResult()
        {
            _originSql = new StringBuilder();
            _parameters = new List<EntityParameter>();
            _cache = MapperConfig.ServiceProvider.GetService<ResultCache>();
        }

        /// <summary>
        /// 创建一个TranslationResult类的实例
        /// </summary>
        /// <returns></returns>
        internal static TranslationResult CreateTranslationResult()
        {
            return new TranslationResult();
        }

        /// <summary>
        /// 获取EntityParameter列表
        /// </summary>
        /// <returns></returns>
        internal IList<EntityParameter> GetParameters()
        {
            return _parameters;
        }

        /// <summary>
        /// 追加一个sql语句和一组EntityParameter对象
        /// </summary>
        /// <param name="entityParameters">参数列表</param>
        internal TranslationResult Append(String sql, IEnumerable<EntityParameter> entityParameters = null)
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
        internal RawExecuteResult ExecuteTranslateResult(ExecutionCore executionCore)
        {
            return Execute(executionCore);
        }

        internal void Clear()
        {
            _originSql.Clear();
            _parameters.Clear();
        }

        private RawExecuteResult Execute(ExecutionCore executionCore)
        {
            var executeResult = GetCache();
            var executeType = executionCore.GetExecuteType(this.ToString());
            if (executeType == ExecuteType.SELECT && executeResult == null)
            {
                executeResult = executionCore.Execute(this);
                SetCache(executeResult);
            }

            return executeResult;
        }

        /// <summary>
        /// 获取作为要缓存的sql语句的key
        /// </summary>
        /// <returns></returns>
        private String PrepareCacheKey()
        {
            Parameter.Validate(_originSql);
            var cacheKey = ToString();
            foreach (var item in GetParameters())
            {
                cacheKey = cacheKey.Replace(item.Key, item.Value.ToString());
            }
            return MD.GetMD5(cacheKey);
        }

        /// <summary>
        /// 设置缓存
        /// </summary>
        /// <param name="executeResult">sql执行后原始的执行结果</param>
        private void SetCache(RawExecuteResult executeResult)
        {
            if (_cache != null)
            {
                _cache.Add(PrepareCacheKey(), executeResult);
            }
        }

        /// <summary>
        /// 获取缓存
        /// </summary>
        /// <returns></returns>
        private RawExecuteResult GetCache()
        {
            if (_cache != null)
            {
                var cacheResult = _cache.Get(PrepareCacheKey());
                if (cacheResult != null)
                {
                    return (RawExecuteResult)cacheResult;
                }
            }

            return default;
        }

        /// <summary>
        /// 将sql语句中多余的空格去掉
        /// </summary>
        /// <param name="sql">语句</param>
        /// <returns></returns>
        private String ReformatSql(String sql)
        {
            Parameter.Validate(sql);
            sql = sql.Replace("   ", " ").Replace("  ", " ");
            return sql.Trim();
        }

        public override String ToString()
        {
            return ReformatSql(_originSql.ToString());
        }
    }
}
