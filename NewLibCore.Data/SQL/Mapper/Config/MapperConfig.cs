using System;
using System.Data;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {
        /// <summary>
        /// 初始化一个MapperConfig类的实例
        /// </summary>
        /// <param name="mapperType">映射类型</param>
        private MapperConfig(MapperType mapperType)
        {
            var services = new ServiceCollection()
                .AddTransient<ResultCache, ExecutionResultCache>()
                .AddTransient<StatementStore>()
                .AddTransient<IMapperDbContext, MapperDbContext>()
                .AddSingleton<ILogger, ConsoleLogger>();

            if (mapperType == MapperType.MSSQL)
            {
                services = services.AddTransient<InstanceConfig, MsSqlInstanceConfig>();
            }
            else if (mapperType == MapperType.MYSQL)
            {
                services = services.AddTransient<InstanceConfig, MySqlInstanceConfig>();
            }
            DIProvider = services.BuildServiceProvider();
        }

        /// <summary>
        /// 初始化mysql配置
        /// </summary>
        public static void InitMySql()
        {
            new MapperConfig(MapperType.MYSQL);
        }

        /// <summary>
        /// 初始化mssql配置
        /// </summary>
        public static void InitMsSql()
        {
            new MapperConfig(MapperType.MSSQL);
        }

        /// <summary>
        /// 启用模型验证
        /// </summary>
        /// <value></value>
        public static Boolean EnableModelValidate { get; set; } = true;

        /// <summary>
        /// 事务隔离级别
        /// </summary>
        public static IsolationLevel TransactionLevel { get; set; } = IsolationLevel.Unspecified;

        /// <summary>
        /// 提供依赖注入的对象
        /// </summary>
        internal static ServiceProvider DIProvider { get; private set; }
    }
}
