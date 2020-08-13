using System;
using System.Data.Common;
using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.EMapper;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Template
{
    /// <summary>
    /// mysql数据库sql模板配置
    /// </summary>
    internal class MySqlTemplate : TemplateBase
    {
        internal override String CreateUpdate<TModel>(TModel model)
        {
            var (tableName, aliasName) = model.GetTableName();
            return $@"UPDATE {tableName} AS {aliasName} SET {model.SqlPart.UpdatePlaceHolders}";
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
            Parameter.IfNullOrZero(predicateType);
            Parameter.IfNullOrZero(left);
            Parameter.IfNullOrZero(right);

            return String.Format(PredicateMapper[predicateType], left, right);
        }

        internal override String CreatePagination(PaginationExpressionMapper pagination, String orderBy, String rawSql)
        {
            Parameter.IfNullOrZero(pagination.Size);
            Parameter.IfNullOrZero(orderBy);
            Parameter.IfNullOrZero(rawSql);

            if (pagination.MaxKey > 0)
            {
                return $@"{rawSql} AND {pagination.QueryMainTable.Value}.{PrimaryKeyName}<{pagination.MaxKey} {orderBy} LIMIT {pagination.Size} ;";
            }

            return $@"{rawSql} {orderBy} LIMIT {pagination.Size * (pagination.Index - 1)},{pagination.Size} ;";
        }

        internal override DbParameter CreateParameter()
        {
            return new MySqlParameter();
        }

        internal override DbConnection CreateDbConnection()
        {
            return new MySqlConnection(Host.GetHostVar(EntityMapperConfig.ConnectionStringName));
        }

        internal override String Identity
        {
            get
            {
                return "; SELECT CAST(@@IDENTITY AS SIGNED) ;";
            }
        }

        internal override String AffectedRows
        {
            get
            {
                return "; SELECT CAST(ROW_COUNT() AS SIGNED) ;";
            }
        }
    }
}
