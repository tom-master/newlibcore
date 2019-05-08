using System;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    public class MapperFactory
    {
        private MapperFactory() { }

        public static MapperFactory Factory { get; } = new MapperFactory();

        internal static ILogger Logger { get; private set; }

        internal static MapperCache Cache { get; private set; }

        internal static MapperInstance Mapper { get; private set; }

        internal static Boolean ExpressionCache { get; private set; } = false;

        internal static Boolean StatementCache { get; private set; } = false;

        public MapperFactory InitLogger(ILogger logger = null)
        {
            Logger = logger ?? new ConsoleLogger();
            return this;
        }

        public MapperFactory SwitchToMySql()
        {
            SwitchTo(DatabaseType.MYSQL);
            return this;
        }

        public MapperFactory SwitchToMsSql()
        {
            SwitchTo(DatabaseType.MSSQL);
            return this;
        }

        public MapperFactory UseExpressionCache()
        {
            ExpressionCache = true;
            return this;
        }

        public MapperFactory UseStatementCache()
        {
            if (!StatementCache)
            {
                StatementCache = true;
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
