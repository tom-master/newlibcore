using System;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    public class MapperFactory
    {
        private static MapperFactory _mapperInstance;

        private static readonly Object _obj = new Object();

        private MapperFactory() { }

        internal static ILogger Logger { get; private set; }

        internal static MapperCache Cache { get; private set; }

        internal static MapperInstance Mapper { get; private set; }

        internal static Boolean ExpressionCache { get; private set; } = false;

        public static MapperFactory GetFactoryInstance()
        {
            if (_mapperInstance == null)
            {
                lock (_obj)
                {
                    if (_mapperInstance == null)
                    {
                        _mapperInstance = new MapperFactory();
                    }
                }
            }
            return _mapperInstance;
        }

        public MapperFactory InitLogger(ILogger logger = null)
        {
            if (Logger == null)
            {
                Logger = logger ?? new ConsoleLogger();
            }
            return this;
        }

        public MapperFactory SwitchToMySql()
        {
            if (Mapper == null)
            {
                SwitchTo(DatabaseType.MYSQL);
            }
            return this;
        }

        public MapperFactory SwitchToMsSql()
        {
            if (Mapper == null)
            {
                SwitchTo(DatabaseType.MSSQL);
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

        private static void SwitchTo(DatabaseType database)
        {
            switch (database)
            {
                case DatabaseType.MSSQL:
                {
                    Mapper = new MsSqlInstance();
                    break;
                }
                case DatabaseType.MYSQL:
                {
                    Mapper = new MySqlInstance();
                    break;
                }
                default:
                    break;
            }
        }
    }

}
