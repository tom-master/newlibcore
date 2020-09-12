using System;
using System.Data.Common;
using System.Data.SqlClient;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Store;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Template
{
    /// <summary>
    /// mssql数据库sql模板配置
    /// </summary>
    internal class MsSqlTemplate : TemplateBase
    {
        internal override String CreateUpdate<TModel>(TModel model)
        {
            var (tableName, aliasName) = model.GetEntityBaseAliasName();
            return $@"UPDATE {aliasName} SET {model.SqlPart.UpdatePlaceHolders} FROM {tableName} AS {aliasName}";
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

            String sql;
            if (EntityMapperConfig.MsSqlPaginationVersion == MsSqlPaginationVersion.GREATERTHAN2012)
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

        internal override DbParameter CreateParameter(String key, Object value, Type dataType)
        {
            return new SqlParameter
            {
                ParameterName = key,
                Value = value,
                DbType = ConvertToDatabaseDataType(dataType)
            };
        }

        internal override DbConnection CreateDbConnection()
        {
            return new SqlConnection(ConfigReader.GetHostVar(EntityMapperConfig.ConnectionStringName));
        }
    }
}
