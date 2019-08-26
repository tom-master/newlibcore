using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {
        private static readonly Object _obj = new Object();

        private MapperConfig(MapperType mapperType)
        {
            var services = new ServiceCollection().AddTransient<ExecutionCore>();
            if (mapperType == MapperType.MSSQL)
            {
                services = services.AddSingleton<InstanceConfig, MsSqlInstanceConfig>();
            }
            else if (mapperType == MapperType.MYSQL)
            {
                services = services.AddSingleton<InstanceConfig, MySqlInstanceConfig>();
            }
            ServiceProvider = services
                .AddSingleton<ResultCache, ExecutionResultCache>()
                .AddSingleton<ILogger, ConsoleLogger>()
                .BuildServiceProvider();
        }

        /// <summary>
        /// 初始化映射配置
        /// </summary>
        /// <param name="mapperType"></param>
        public static void InitMapper(MapperType mapperType = MapperType.MYSQL)
        {
            new MapperConfig(mapperType);
        }

        internal static IServiceProvider ServiceProvider { get; private set; }
    }
}
