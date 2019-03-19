using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Data.Common;
using System.Data.SqlClient;

namespace NewLibCore.Data.SQL.MapperConfig
{
    public static class SwitchDatabase
    {
        internal static String ConnectionString { get { return Host.GetHostVar("database"); } }

        public static DatabaseType Type { get; private set; }

        internal static DatabaseSyntaxBuilder DatabaseSyntax { get; private set; }

        static SwitchDatabase()
        {
            Type = DatabaseType.NONE;
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
                    DatabaseSyntax = new MsSqlSyntaxBuilder
                    {
                        IdentitySuffix = " SELECT @@IDENTITY",
                        RowCountSuffix = " SELECT @@ROWCOUNT",
                        Page = " OFFSET ({value}) ROWS FETCH NEXT {pageSize} ROWS ONLY"
                    };
                    break;
                }
                case DatabaseType.MYSQL:
                {
                    DatabaseSyntax = new MySqlSyntaxBuilder
                    {
                        IdentitySuffix = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ",
                        RowCountSuffix = " ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c",
                        Page = " LIMIT {value},{pageSize}"
                    };
                    break;
                }
                default:
                    break;
            }

            Type = database;
        }

        internal static DbConnection GetConnectionInstance()
        {
            var connection = Host.GetHostVar("database");

            switch (Type)
            {
                case DatabaseType.MSSQL:
                {
                    return new SqlConnection(connection);
                }
                case DatabaseType.MYSQL:
                {
                    return new MySqlConnection(connection);
                }
                default:
                {
                    throw new ArgumentException($@"暂不支持的数据库类型:{Type}");
                }
            }
        }
    }

}
