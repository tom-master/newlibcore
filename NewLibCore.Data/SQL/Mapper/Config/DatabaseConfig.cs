using System;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    public class MapperFactory
    {
        private static readonly Object _obj = new Object();

        private static MapperFactory _mapperFactory;

        private MapperFactory() { }


        public static MapperFactory Factory
        {
            get
            {
                if (_mapperFactory == null)
                {
                    lock (_obj)
                    {
                        if (_mapperFactory == null)
                        {
                            _mapperFactory = new MapperFactory();
                        }
                    }
                }
                return _mapperFactory;
            }
        }

        internal static MapperInstance Instance { get; private set; }

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
            if (Instance != null)
            {
                Instance.Cache = new StatementCache();
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
