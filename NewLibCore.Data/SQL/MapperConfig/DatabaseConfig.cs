using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Data.Common;
using System.Data.SqlClient;

namespace NewLibCore.Data.SQL.MapperConfig
{
    public class DatabaseConfig
    {
        internal static DatabaseInstance Instance { get; private set; }

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
