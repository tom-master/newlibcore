using System;
using System.Data.Common;
using MySql.Data.MySqlClient;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Template
{
    /// <summary>
    /// mysql数据库sql模板配置
    /// </summary>
    internal class MySqlTemplate : TemplateBase
    {
        internal override String UpdateTemplate
        {
            get
            {
                return "UPDATE {0} AS {1} SET {2} ";
            }
        }

        protected override void AppendPredicateType()
        {
            PredicateMapper.Add(PredicateType.FULL_LIKE, "{0} LIKE CONCAT('%',{1},'%')");
            PredicateMapper.Add(PredicateType.START_LIKE, "{0} LIKE CONCAT('',{1},'%')");
            PredicateMapper.Add(PredicateType.END_LIKE, "{0} LIKE CONCAT('%',{1},'')");
            PredicateMapper.Add(PredicateType.IN, "FIND_IN_SET({0},{1})");
        }

        internal override String CreatePredicate(PredicateType predicateType, String left, String right)
        {
            Parameter.Validate(predicateType);
            Parameter.Validate(left);
            Parameter.Validate(right);

            return String.Format(PredicateMapper[predicateType], left, right);
        }

        internal override String CreatePagination(Int32 pageIndex, Int32 pageSize, String orderBy,String rawSql)
        {
            Parameter.Validate(pageSize);
            Parameter.Validate(orderBy);
            Parameter.Validate(rawSql);

            return $@"{rawSql} {orderBy} LIMIT {pageSize * (pageIndex - 1)},{pageSize} ;";
        }

        internal override DbParameter CreateParameter()
        {
            return new MySqlParameter();
        }

        internal override DbConnection CreateDbConnection()
        {
            return new MySqlConnection(Host.GetHostVar(MapperConfig.ConnectionStringName));
        }

        internal override String Identity
        {
            get
            {
                return "; SELECT CAST(@@IDENTITY AS SIGNED) AS c ;";
            }
        }

        internal override String RowCount
        {
            get
            {
                return "; SELECT CAST(ROW_COUNT() AS SIGNED) AS c ;";
            }
        }
    }
}
