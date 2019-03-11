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

        public static DatabaseSyntaxBuilder DatabaseSyntax { get; private set; }

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
                    DatabaseSyntax = new MsSqlSyntaxBuilder();
                    return new SqlConnection(connection);
                }
                case Database.MYSQL:
                {
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
