using System;
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
                .AddTransient<SegmentManager>()
                .AddTransient<DbContext>()
                .AddSingleton<ILogger, ConsoleLogger>();

            if (mapperType == MapperType.MSSQL)
            {
                services = services.AddTransient<InstanceConfig, MsSqlInstanceConfig>();
            }
            else if (mapperType == MapperType.MYSQL)
            {
                services = services.AddTransient<InstanceConfig, MySqlInstanceConfig>();
            }
            ServiceProvider = services.BuildServiceProvider();
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
        public static Boolean EnableModelValidate { get; set; }

        /// <summary>
        /// 提供依赖注入的对象
        /// </summary>
        internal static ServiceProvider ServiceProvider { get; private set; }
    }
}
