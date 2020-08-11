using System;
using System.Data;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.ProcessorFactory;
using NewLibCore.Data.SQL.Template;
using NewLibCore.Logger;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.EMapper
{
    public class EntityMapperConfig
    {
        private static ILogger _logger;

        public static IServiceProvider Provider { get; private set; }

        /// <summary>
        /// 连接字符串名称
        /// </summary>
        /// <value></value>
        public static String ConnectionStringName { get; set; }

        /// <summary>
        /// 是否在出现异常时抛出异常
        /// </summary>
        /// <value></value>
        public static Boolean ThrowException { get; set; } = true;

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
        internal static MsSqlPaginationVersion MsSqlPaginationVersion { get; set; } = MsSqlPaginationVersion.NONE;

        /// <summary>
        /// 初始化默认配置
        /// </summary>
        public static void InitDefaultSetting()
        {
            UseMySql();
            SetTransactionLevel(IsolationLevel.Unspecified);
            EnableModelValidate = true;
            InitDependency();
        }

        /// <summary>
        /// 初始化依赖注入
        /// </summary>
        private static void InitDependency()
        {
            IServiceCollection services = new ServiceCollection();

            #region scoped
            if (MapperType == MapperType.NONE)
            {
                throw new Exception("没有指定要映射的数据库类型");
            }
            if (MapperType == MapperType.MSSQL)
            {
                services = services.AddTransient<TemplateBase, MsSqlTemplate>();
            }
            else if (MapperType == MapperType.MYSQL)
            {
                services = services.AddTransient<TemplateBase, MySqlTemplate>();
            }

            services = services.AddTransient<MapperDbContextBase, MapperDbContext>();

            #endregion

            #region singleton

            RunDiagnosis.SetLoggerInstance(_logger ?? new DefaultLogger());
            #endregion

            #region transient

            services = services.AddTransient<ExpressionProcessor, DefaultExpressionProcessor>();
            services = services.AddTransient<ExpressionProcessorResult>();

            services = services.AddTransient<Processor, RawSqlProcessor>();
            services = services.AddTransient<Processor, QueryProcessor>();
            services = services.AddTransient<Processor, UpdateProcessor>();
            services = services.AddTransient<Processor, InsertProcessor>();

            #endregion

            Provider = services.BuildServiceProvider();
        }

        /// <summary>
        /// 切换为mysql
        /// </summary>
        public static void UseMySql()
        {
            MapperType = MapperType.MYSQL;
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        public static void UseMsSql()
        {
            MapperType = MapperType.MSSQL;
        }

        /// <summary>
        /// 设置事务隔离级别
        /// </summary>
        /// <param name="isolationLevel"></param>
        public static void SetTransactionLevel(IsolationLevel isolationLevel)
        {
            TransactionLevel = isolationLevel;
        }

        /// <summary>
        /// 设置自定义日志记录组件
        /// </summary>
        /// <param name="logger"></param>
        public static void SetLogger(ILogger logger)
        {
            Parameter.IfNullOrZero(logger);
            _logger = logger;
        }
    }
}
