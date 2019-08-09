using System;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {
        private static ILogger _logger;

        private static readonly Object _obj = new Object();

        private MapperConfig() { }

        internal static InstanceConfig Instance { get; private set; }

        /// <summary>
        /// 设置日志
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public static void SetLogger(ILogger logger)
        {
            _logger = logger;
        }

        /// <summary>
        /// 切换为mysql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public static void SwitchToMySql(Boolean cache = false)
        {
            SwitchTo(DatabaseType.MYSQL, cache);
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public static void SwitchToMsSql(Boolean cache = false)
        {
            SwitchTo(DatabaseType.MSSQL, cache);
        }

        /// <summary>
        /// 切换到指定数据库配置实例
        /// </summary>
        /// <param name="database"></param>
        /// <param name="logger"></param>
        private static void SwitchTo(DatabaseType database, Boolean cache)
        {
            if (Instance == null)
            {
                lock (_obj)
                {
                    if (Instance == null)
                    {
                        switch (database)
                        {
                            case DatabaseType.MSSQL:
                            {
                                Instance = new MsSqlInstanceConfig(_logger);
                                break;
                            }
                            case DatabaseType.MYSQL:
                            {
                                Instance = new MySqlInstanceConfig(_logger);
                                break;
                            }
                            default:
                            {
                                throw new ArgumentException($@"暂不支持的数据库类型:{database}");
                            }
                        }
                        if (cache)
                        {
                            Instance.UseCache();
                        }
                    }
                }
            }
        }
    }
}
