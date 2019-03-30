﻿using NewLibCore.Data.SQL.MapperExtension;

namespace NewLibCore.Data.SQL.MapperConfig
{
    public class MapperFactory
    {
        internal static MapperInstance Instance { get; private set; }

        internal static ILogger Logger { get; private set; }

        public static void InitLogger(ILogger logger = null)
        {
            Logger = logger ?? new ConsoleLogger();
        }

        public static void SwitchToMySql()
        {
            SwitchTo(DatabaseType.MYSQL);
        }

        public static void SwitchToMsSql()
        {
            SwitchTo(DatabaseType.MSSQL);
        }

        private static void SwitchTo(DatabaseType database)
        {
            switch (database)
            {
                case DatabaseType.MSSQL:
                {
                    Instance = new MsSqlInstance();
                    break;
                }
                case DatabaseType.MYSQL:
                {
                    Instance = new MsSqlInstance();
                    break;
                }
                default:
                    break;
            }
        }
    }

}
