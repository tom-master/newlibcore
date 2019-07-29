using System;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {
        private static readonly Object _obj = new Object();

        private MapperConfig() { }

        internal static InstanceConfig DatabaseConfig { get; private set; }

        /// <summary>
        /// 初始化数据库配置
        /// </summary>
        /// <returns></returns>
        public static MapperConfig Instance { get; } = new MapperConfig();

        /// <summary>
        /// 切换为mysql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public MapperConfig SwitchToMySql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MYSQL, logger);
            return this;
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public MapperConfig SwitchToMsSql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MSSQL, logger);
            return this;
        }

        public void UseCache()
        {
            DatabaseConfig?.UseCache();
        }

        /// <summary>
        /// 切换到指定数据库配置实例
        /// </summary>
        /// <param name="database"></param>
        /// <param name="logger"></param>
        private void SwitchTo(DatabaseType database, ILogger logger)
        {
            if (DatabaseConfig == null)
            {
                lock (_obj)
                {
                    if (DatabaseConfig == null)
                    {
                        switch (database)
                        {
                            case DatabaseType.MSSQL:
                            {
                                DatabaseConfig = new MsSqlInstanceConfig(logger);
                                break;
                            }
                            case DatabaseType.MYSQL:
                            {
                                DatabaseConfig = new MySqlInstanceConfig(logger);
                                break;
                            }
                            default:
                            {
                                throw new ArgumentException($@"暂不支持的数据库类型:{database}");
                            }
                        }
                    }
                }
            }
        }
    }
}
