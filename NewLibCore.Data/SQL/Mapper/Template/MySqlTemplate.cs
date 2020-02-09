using System;
using System.Data.Common;
using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.Mapper.Component.Cache;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Template
{
    /// <summary>
    /// mysql数据库sql模板配置
    /// </summary>
    internal class MySqlTemplate : TemplateBase
    {
        internal override String CreateUpdate(String tableName, String aliasName, String field)
        {
            return String.Format("UPDATE {0} AS {1} SET {2} ", tableName, aliasName, field);
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

        internal override String CreatePagination(PaginationExpressionMapper pagination, String orderBy, String rawSql)
        {
            Parameter.Validate(pagination.Size);
            Parameter.Validate(orderBy);
            Parameter.Validate(rawSql);

            if (pagination.MaxKey > 0)
            {
                return $@"{rawSql} AND {pagination.QueryMainTable.Value}.{PrimaryKey}<{pagination.MaxKey} {orderBy} LIMIT {pagination.Size} ;";
            }

            return $@"{rawSql} {orderBy} LIMIT {pagination.Size * (pagination.Index - 1)},{pagination.Size} ;";
        }

        internal override DbParameter CreateParameter()
        {
            return new MySqlParameter();
        }

        internal override DbConnection CreateDbConnection()
        {
            return new MySqlConnection(Host.GetHostVar(EntityMapper.ConnectionStringName));
        }

        internal override String Identity
        {
            get
            {
                return "; SELECT CAST(@@IDENTITY AS SIGNED) AS c ;";
            }
        }

        internal override String AffectedRows
        {
            get
            {
                return "; SELECT CAST(ROW_COUNT() AS SIGNED) AS c ;";
            }
        }
    }
}
