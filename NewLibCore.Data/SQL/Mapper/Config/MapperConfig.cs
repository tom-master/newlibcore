using System;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 数据库配置
    /// </summary>
    public class MapperConfig
    {
        private static readonly Object _obj = new Object();

        private static MapperConfig _mapperConfig;

        private MapperConfig() { }

        internal InstanceConfig DatabaseInstance { get; private set; }

        /// <summary>
        /// 初始化数据库配置
        /// </summary>
        /// <returns></returns>
        public static MapperConfig GetInstance()
        {
            if (_mapperConfig == null)
            {
                lock (_obj)
                {
                    if (_mapperConfig == null)
                    {
                        _mapperConfig = new MapperConfig();
                    }
                }
            }
            return _mapperConfig;
        }

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

        /// <summary>
        /// 使用缓存
        /// </summary>
        /// <returns></returns>
        public MapperConfig UseCache()
        {
            if (DatabaseInstance.Cache == null)
            {
                lock (_obj)
                {
                    if (DatabaseInstance.Cache == null)
                    {
                        DatabaseInstance.Cache = new ExecutionResultCache();
                    }
                }
            }
            return this;
        }

        /// <summary>
        /// 切换到指定数据库配置实例
        /// </summary>
        /// <param name="database"></param>
        /// <param name="logger"></param>
        private void SwitchTo(DatabaseType database, ILogger logger)
        {
            if (DatabaseInstance == null)
            {
                lock (_obj)
                {
                    if (DatabaseInstance == null)
                    {
                        switch (database)
                        {
                            case DatabaseType.MSSQL:
                            {
                                DatabaseInstance = new MsSqlInstanceConfig(logger);
                                break;
                            }
                            case DatabaseType.MYSQL:
                            {
                                DatabaseInstance = new MySqlInstanceConfig(logger);
                                break;
                            }
                            default:
                                break;
                        }
                    }
                }
            }
        }
    }
}
