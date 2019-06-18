using System;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    public class DatabaseConfigFactory
    {
        private static readonly Object _obj = new Object();

        private static DatabaseConfigFactory _databaseConfigFactory;

        private DatabaseConfigFactory() { }

        internal static DatabaseInstanceConfig Instance { get; private set; }

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

        public DatabaseConfigFactory SwitchToMySql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MYSQL, logger);
            return this;
        }

        public DatabaseConfigFactory SwitchToMsSql(ILogger logger = null)
        {
            SwitchTo(DatabaseType.MSSQL, logger);
            return this;
        }

        public DatabaseConfigFactory UseCache()
        {
            if (Instance.Cache == null)
            {
                lock (_obj)
                {
                    if (Instance.Cache == null)
                    {
                        Instance.Cache = new StatementCache();
                    }
                }
            }
            return this;
        }

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
