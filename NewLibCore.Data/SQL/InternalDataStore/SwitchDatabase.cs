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

        internal static String Page { get; private set; }

        internal static String ConnectionString { get { return Host.GetHostVar("database"); } }

        public static void SwitchTo(Database database)
        {
            _database = database;
        }

        internal static DbConnection GetConnectionInstance()
        {
            var connection = Host.GetHostVar("database");
            switch (_database)
            {
                case Database.MSSQL:
                {
                    IdentitySuffix = " SELECT @@IDENTITY";
                    RowCountSuffix = " SELECT @@ROWCOUNT";
                    Page = @"SELECT TOP ( @pageSize * @pageIndex ) *
                             FROM    ( SELECT 
                                                ROW_NUMBER() OVER ( ORDER BY dbo.Products.UnitPrice DESC ) AS rownum ,
                                                *
                                      FROM      dbo.Products
                                    ) AS temp
                             WHERE   temp.rownum > ( @pageSize * ( @pageIndex - 1 ) )
                             ORDER BY temp.UnitPrice";

                    DatabaseSyntax = new MsSqlSyntaxBuilder();
                    return new SqlConnection(connection);
                }
                case Database.MYSQL:
                {
                    IdentitySuffix = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ";
                    RowCountSuffix = " ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c";
                    Page = "LIMIT {0},{1}";

                    DatabaseSyntax = new MySqlSyntaxBuilder();
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
