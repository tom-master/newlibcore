using System;
using System.Data.Common;
using System.Data.SqlClient;
using System.Text;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Template
{
    /// <summary>
    /// mssql数据库sql模板配置
    /// </summary>
    internal class MsSqlTemplate : TemplateBase
    {
        public MsSqlTemplate() : base() { }

        private MsSqlPaginationVersion _mssqlPaginationVersion;

        internal override StringBuilder CreateUpdate<TModel>(TModel model)
        {
            var (tableName, aliasName) = model.GetEntityBaseAliasName();
            var s = $@"UPDATE {aliasName} SET {model.GetSqlElements().UpdatePlaceHolders} FROM {tableName} AS {aliasName}";
            return new StringBuilder(s);
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
            Check.IfNullOrZero(predicateType);
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(right);

            return String.Format(PredicateMapper[predicateType], left, right);
        }

        protected override void SetVersion(int version)
        {
            if (version <= 11)
            {
                _mssqlPaginationVersion = MsSqlPaginationVersion.LESSTHEN2012;
            }
            else
            {
                _mssqlPaginationVersion = MsSqlPaginationVersion.GREATERTHAN2012;
            }
        }

        internal override void CreatePagination(PaginationComponent pagination, String orderBy, StringBuilder rawSql)
        {
            Check.IfNullOrZero(pagination.Size);
            Check.IfNullOrZero(orderBy);
            Check.IfNullOrZero(rawSql);

            String sql = "";
            if (_mssqlPaginationVersion == MsSqlPaginationVersion.GREATERTHAN2012)
            {
                rawSql = rawSql.Append($@" {orderBy} OFFSET ({pagination.Index * pagination.Size}) ROWS FETCH NEXT {pagination.Size} ROWS ONLY ;");
            }
            else
            {

                sql = rawSql.ToString();
                sql = $@" SELECT * FROM ( {sql.Insert(sql.IndexOf(" "), $@" TOP({pagination.Size}) ROW_NUMBER() OVER({orderBy}) AS rownumber,")} ) AS temprow WHERE temprow.rownumber>({pagination.Size}*({pagination.Index}-1)) {orderBy}";
            }
            rawSql = new StringBuilder(sql);
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

        internal override DbConnection CreateDbConnection(String connectionString)
        {
            return new SqlConnection(connectionString);
        }
    }
}
