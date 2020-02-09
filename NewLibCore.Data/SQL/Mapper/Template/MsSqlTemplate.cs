using System;
using System.Data.Common;
using System.Data.SqlClient;
using NewLibCore.Data.SQL.Mapper.Component.Cache;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Template
{
    /// <summary>
    /// mssql数据库sql模板配置
    /// </summary>
    internal class MsSqlTemplate : TemplateBase
    {

        internal override String CreateUpdate(String tableName, String aliasName, String field)
        {
            return String.Format("UPDATE {0} SET {1} FROM {2} AS {0} ", aliasName, field, tableName);
        }

        internal override String Identity
        {
            get
            {
                return " SELECT @@IDENTITY ;";
            }
        }

        internal override String AffectedRows
        {
            get
            {
                return " SELECT @@ROWCOUNT ;";
            }
        }

        protected override void AppendPredicateType()
        {
            PredicateMapper.Add(PredicateType.FULL_LIKE, "{0} LIKE '%{1}%'");
            PredicateMapper.Add(PredicateType.START_LIKE, "{0} LIKE '{1}%'");
            PredicateMapper.Add(PredicateType.END_LIKE, "{0} LIKE '%{1}' ");
            PredicateMapper.Add(PredicateType.IN, "{0} IN ({1})");
        }

        internal override String CreatePredicate(PredicateType predicateType, String left, String right)
        {
            Parameter.Validate(predicateType);
            Parameter.Validate(left);
            Parameter.Validate(right);

            return String.Format(PredicateMapper[predicateType], left, right);
        }

        internal override String CreatePagination(PaginationExpressionMapper pagination, String orderBy, String rawSql)
        {
            Parameter.Validate(pagination.Size);
            Parameter.Validate(orderBy);
            Parameter.Validate(rawSql);

            var sql = "";
            if (EntityMapper.MsSqlPaginationVersion == MsSqlPaginationVersion.GREATERTHAN2012)
            {
                sql = $@" {rawSql} {orderBy} OFFSET ({pagination.Index * pagination.Size}) ROWS FETCH NEXT {pagination.Size} ROWS ONLY ;";
            }
            else
            {
                sql = rawSql;
                sql = $@" SELECT * FROM ( {sql.Insert(sql.IndexOf(" "), $@" TOP({pagination.Size}) ROW_NUMBER() OVER({orderBy}) AS rownumber,")} ) AS temprow WHERE temprow.rownumber>({pagination.Size}*({pagination.Index}-1)) {orderBy}";
            }
            return sql;
        }

        internal override DbParameter CreateParameter()
        {
            return new SqlParameter();
        }

        internal override DbConnection CreateDbConnection()
        {
            return new SqlConnection(Host.GetHostVar(EntityMapper.ConnectionStringName));
        }
    }
}
