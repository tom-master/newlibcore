using System;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 数据库配置
    /// </summary>
    public class DatabaseConfigFactory
    {
        private static readonly Object _obj = new Object();

        private static DatabaseConfigFactory _databaseConfigFactory;

        private DatabaseConfigFactory() { }

        internal static DatabaseInstanceConfig Instance { get; private set; }

        /// <summary>
        /// 初始化数据库配置
        /// </summary>
        /// <returns></returns>
        public static DatabaseConfigFactory Init()
        {
            if (_databaseConfigFactory == null)
            {
                lock (_obj)
                {
                    if (_databaseConfigFactory == null)
                    {
                        _databaseConfigFactory = new DatabaseConfigFactory();
                    }
                }
            }
            return _databaseConfigFactory;
        }

        /// <summary>
        /// 切换为mysql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public DatabaseConfigFactory SwitchToMySql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MYSQL, logger);
            return this;
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public DatabaseConfigFactory SwitchToMsSql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MSSQL, logger);
            return this;
        }

        /// <summary>
        /// 使用缓存
        /// </summary>
        /// <returns></returns>
        public DatabaseConfigFactory UseCache()
        {
            if (Instance.Cache == null)
            {
                lock (_obj)
                {
                    if (Instance.Cache == null)
                    {
                        Instance.Cache = new StatementResultCache();
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
        private static void SwitchTo(DatabaseType database, ILogger logger)
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
                                    Instance = new MsSqlInstanceConfig(logger);
                                    break;
                                }
                            case DatabaseType.MYSQL:
                                {
                                    Instance = new MySqlInstanceConfig(logger);
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
