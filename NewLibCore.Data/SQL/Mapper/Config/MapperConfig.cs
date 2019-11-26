using System;
using System.Data;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Logger;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {

        private static Func<ILogger> _logger = () => null;

        private static Func<ResultCache> _cache = () => null;

        /// <summary>
        /// 连接字符串名称
        /// </summary>
        /// <value></value>
        public static String ConnectionStringName { get; set; }

        /// <summary>
        /// 日志
        /// </summary>
        public static ILogger Logger
        {
            get
            {
                if (_logger == null)
                {
                    throw new Exception();
                }
                return _logger();
            }
        }

        public static ResultCache Cache
        {
            get
            {
                if (_cache == null)
                {
                    throw new Exception();
                }
                return _cache();
            }
        }

        /// <summary>
        /// 启用模型验证
        /// </summary>
        /// <value></value>
        public static Boolean EnableModelValidate { get; set; }

        /// <summary>
        /// 事务隔离级别
        /// </summary>
        internal static IsolationLevel TransactionLevel { get; set; }

        /// <summary>
        /// 映射的数据库类型
        /// </summary>
        internal static MapperType MapperType { get; set; }

        /// <summary>
        /// mssql的版本
        /// </summary>
        internal static MsSqlPaginationVersion MsSqlPaginationVersion { get; set; } = MsSqlPaginationVersion.None;

        /// <summary>
        /// 初始化默认配置
        /// </summary>
        public static void InitDefaultSetting()
        {
            UseMySql();
            SetTransactionLevel(IsolationLevel.Unspecified);
            EnableModelValidate = true;

            _logger = () => new DefaultLogger();
            _cache = () => new DefaultResultCache();
        }

        public static void UseMySql()
        {
            MapperType = MapperType.MYSQL;
        }

        public static void UseMsSql()
        {
            MapperType = MapperType.MSSQL;
        }

        public static void SetTransactionLevel(IsolationLevel isolationLevel)
        {
            TransactionLevel = isolationLevel;
        }

        public static void SetLogger(ILogger logger)
        {
            Parameter.Validate(logger);
            _logger = () => logger;
        }

        public static void SetCache(ResultCache cache)
        {
            Parameter.Validate(cache);
            _cache = () => cache;
        }
    }
}
