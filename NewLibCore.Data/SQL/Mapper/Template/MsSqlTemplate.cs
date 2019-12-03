using System;
using System.Data.Common;
using System.Data.SqlClient;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Template
{
    /// <summary>
    /// mssql数据库sql模板配置
    /// </summary>
    internal class MsSqlTemplate : TemplateBase
    {
        internal override String UpdateTemplate
        {
            get
            {
                return "UPDATE {0} SET {1} FROM {2} AS {0} ";
            }
        }

        internal override String Identity
        {
            get
            {
                return " SELECT @@IDENTITY ;";
            }
        }

        internal override String RowCount
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

        internal override String CreatePagination(Int32 pageIndex, Int32 pageSize, String orderBy, String rawSql)
        {
            Parameter.Validate(pageSize);
            Parameter.Validate(orderBy);
            Parameter.Validate(rawSql);

            var sql = "";
            if (MapperConfig.MsSqlPaginationVersion == MsSqlPaginationVersion.GREATERTHAN2012)
            {
                sql = $@" {rawSql} {orderBy} OFFSET ({pageIndex * pageSize}) ROWS FETCH NEXT {pageSize} ROWS ONLY ;";
            }
            else
            {
                sql = rawSql;
                sql = $@" SELECT * FROM ( {sql.Insert(sql.IndexOf(" "), $@" TOP({pageSize}) ROW_NUMBER() OVER({orderBy}) AS rownumber,")} ) AS temprow WHERE temprow.rownumber>({pageSize}*({pageIndex}-1)) {orderBy}";
            }
            return sql;
        }

        internal override DbParameter CreateParameter()
        {
            return new SqlParameter();
        }

        internal override DbConnection CreateDbConnection()
        {
            return new SqlConnection(Host.GetHostVar(MapperConfig.ConnectionStringName));
        }
    }
}
