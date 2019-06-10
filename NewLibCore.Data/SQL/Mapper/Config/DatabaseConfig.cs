using System;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    public class MapperFactory
    {
        private static readonly Object _obj = new Object();

        private MapperFactory() { }

        static MapperFactory()
        {
            Factory = new MapperFactory();
        }

        internal static MapperCache Cache { get; private set; }

        internal static MapperInstance Instance { get; private set; }

        public static MapperFactory Factory { get; private set; }

        public MapperFactory SwitchToMySql(ILogger logger = null)
        {
            if (Instance == null)
            {
                SwitchTo(DatabaseType.MYSQL, logger);
            }
            return this;
        }

        public MapperFactory SwitchToMsSql(ILogger logger = null)
        {
            if (Instance == null)
            {
                SwitchTo(DatabaseType.MSSQL, logger);
            }
            return this;
        }

        public MapperFactory UseCache()
        {
            if (Cache == null)
            {
                Cache = new StatementCache();
            }
            return this;
        }

        private static void SwitchTo(DatabaseType database, ILogger logger)
        {
            switch (database)
            {
                case DatabaseType.MSSQL:
                {
                    Instance = new MsSqlInstance(logger);
                    break;
                }
                case DatabaseType.MYSQL:
                {
                    Instance = new MySqlInstance(logger);
                    break;
                }
                default:
                    break;
            }
        }
    }

}
