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

        private static MapperConfig _config;

        private MapperConfig() { }

        internal static InstanceConfig DatabaseConfig { get; private set; }

        /// <summary>
        /// 初始化数据库配置
        /// </summary>
        /// <returns></returns>
        public static MapperConfig Instance
        {
            get
            {
                if (_config == null)
                {
                    lock (_obj)
                    {
                        if (_config == null)
                        {
                            _config = new MapperConfig();
                        }
                    }
                }
                return _config;
            }
        }

        /// <summary>
        /// 切换为mysql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public void SwitchToMySql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MYSQL, logger);
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public void SwitchToMsSql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MSSQL, logger);
        }

        /// <summary>
        /// 切换到指定数据库配置实例
        /// </summary>
        /// <param name="database"></param>
        /// <param name="logger"></param>
        private void SwitchTo(DatabaseType database, ILogger logger)
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
