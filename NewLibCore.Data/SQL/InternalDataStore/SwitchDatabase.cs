using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.BuildExtension;
using System;
using System.Data.Common;
using System.Data.SqlClient;

namespace NewLibCore.Data.SQL.InternalDataStore
{
    public class SwitchDatabase
    {
        private static Database _database;

        private static DbConnection _dbConnection;

        private static readonly Object _sync = new Object();

        internal static DatabaseSyntaxBuilder DatabaseSyntax { get; private set; }

        internal static String ConnectionString { get { return Host.GetHostVar("database"); } }

        private SwitchDatabase() { }

        public static void SwitchTo(Database database)
        {
            switch (database)
            {
                case Database.MSSQL:
                {
                    DatabaseSyntax = new MsSqlSyntaxBuilder
                    {
                        IdentitySuffix = " SELECT @@IDENTITY",
                        RowCountSuffix = " SELECT @@ROWCOUNT",
                        Page = " OFFSET ({value}) ROWS FETCH NEXT {pageSize} ROWS ONLY"
                    };
                    break;
                }
                case Database.MYSQL:
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

            _database = database;
        }

        internal static DbConnection GetConnectionInstance()
        {
            var connection = Host.GetHostVar("database");

            if (_dbConnection == null)
            {
                lock (_sync)
                {
                    if (_dbConnection == null)
                    {
                        switch (_database)
                        {
                            case Database.MSSQL:
                            {
                                _dbConnection = new SqlConnection(connection);
                                return _dbConnection;
                            }
                            case Database.MYSQL:
                            {
                                _dbConnection = new MySqlConnection(connection);
                                return _dbConnection;
                            }
                            default:
                                throw new ArgumentException();
                        }
                    }
                }
            }
            return _dbConnection;
        }
    }

    public enum Database
    {
        MSSQL = 1,
        MYSQL = 2
    }
}
