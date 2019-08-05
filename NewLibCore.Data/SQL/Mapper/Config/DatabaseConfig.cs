using System;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {
        private ILogger _logger;

        private static readonly Object _obj = new Object();

        private MapperConfig() { }

        internal static InstanceConfig DatabaseConfig { get; private set; }

        /// <summary>
        /// 初始化数据库配置
        /// </summary>
        /// <returns></returns>
        public static MapperConfig Instance { get; } = new MapperConfig();

        public Action OpenTransaction { get; internal set; }

        public Action Commit { get; internal set; }

        public Action Rollback { get; internal set; }

        /// <summary>
        /// 设置日志
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public MapperConfig SetLogger(ILogger logger)
        {
            _logger = logger;
            return this;
        }

        /// <summary>
        /// 切换为mysql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public MapperConfig SwitchToMySql(Boolean cache = false)
        {
            SwitchTo(DatabaseType.MYSQL, cache);
            return this;
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public MapperConfig SwitchToMsSql(Boolean cache = false)
        {
            SwitchTo(DatabaseType.MSSQL, cache);
            return this;
        }

        /// <summary>
        /// 切换到指定数据库配置实例
        /// </summary>
        /// <param name="database"></param>
        /// <param name="logger"></param>
        private void SwitchTo(DatabaseType database, Boolean cache)
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
                                DatabaseConfig = new MsSqlInstanceConfig(_logger);
                                break;
                            }
                            case DatabaseType.MYSQL:
                            {
                                DatabaseConfig = new MySqlInstanceConfig(_logger);
                                break;
                            }
                            default:
                            {
                                throw new ArgumentException($@"暂不支持的数据库类型:{database}");
                            }
                        }
                        DatabaseConfig.InitExecutionCore();
                        if (cache)
                        {
                            DatabaseConfig.UseCache();
                        }
                    }
                }
            }
        }
    }
}
