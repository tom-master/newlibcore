using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.BuildExtension;
using System;
using System.Data.Common;
using System.Data.SqlClient;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public static class SwitchDatabase
    {
        private static Database _database;

        internal static DatabaseSyntaxBuilder DatabaseSyntax { get; private set; }

        internal static String IdentitySuffix { get; private set; }

        internal static String RowCountSuffix { get; private set; }

        public static void SwitchTo(Database database)
        {
            _database = database;
        }

        internal static String ConnectionString { get { return Host.GetHostVar("database"); } }

        internal static DbConnection GetConnectionInstance()
        {
            var connection = Host.GetHostVar("database");
            switch (_database)
            {
                case Database.MSSQL:
                {
                    IdentitySuffix = " SELECT @@IDENTITY";
                    RowCountSuffix = " SELECT @@ROWCOUNT";
                    DatabaseSyntax = new MsSqlSyntaxBuilder();
                    return new SqlConnection(connection);
                }
                case Database.MYSQL:
                {
                    IdentitySuffix = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ";
                    RowCountSuffix = " ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c";
                    DatabaseSyntax = new MysqlSyntaxBuilder();
                    return new MySqlConnection(connection);
                }
                default:
                    throw new ArgumentException();
            }
        }
    }

    public enum Database
    {
        MSSQL = 1,
        MYSQL = 2
    }
}
