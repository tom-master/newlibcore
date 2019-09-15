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
                .AddSingleton<ExecutionCore>()
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
        /// 初始化映射配置
        /// </summary>
        /// <param name="mapperType">映射类型</param>
        public static void InitMapper(MapperType mapperType = MapperType.MYSQL)
        {
            new MapperConfig(mapperType);
        }

        /// <summary>
        /// 提供依赖注入的对象
        /// </summary>
        internal static ServiceProvider ServiceProvider { get; private set; }
    }
}
