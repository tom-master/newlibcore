using System;
using System.Data.Common;
using System.Data.SqlClient;
using System.Text;
using NewLibCore.Storage.SQL.Component;
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

        internal override StringBuilder CreateUpdate(string tableName, string aliasName, string placeHolders)
        {
            var s = $@"UPDATE {aliasName} SET {placeHolders} FROM {tableName} AS {aliasName} <where>";
            return new StringBuilder(s);
        }

        internal override string Identity
        {
            get
            {
                return " SELECT @@IDENTITY ;";
            }
        }

        internal override string AffectedRows
        {
            get
            {
                return " SELECT @@ROWCOUNT ;";
            }
        }

        protected override void AppendPredicateType()
        {
            PredicateMapper.Add(EMType.FULL_LIKE, "{0} LIKE '%{1}%'");
            PredicateMapper.Add(EMType.START_LIKE, "{0} LIKE '{1}%'");
            PredicateMapper.Add(EMType.END_LIKE, "{0} LIKE '%{1}' ");
            PredicateMapper.Add(EMType.IN, "{0} IN ({1})");
        }

        internal override string CreatePredicate(EMType predicateType, string left, string right)
        {
            Check.IfNullOrZero(predicateType);
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(right);

            return string.Format(PredicateMapper[predicateType], left, right);
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

        internal override void CreatePagination(PaginationComponent pagination, string orderBy, StringBuilder rawSql)
        {
            Check.IfNullOrZero(pagination.Size);
            Check.IfNullOrZero(orderBy);
            Check.IfNullOrZero(rawSql);

            string sql = "";
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

        internal override DbParameter CreateParameter(string key, Object value, Type dataType)
        {
            return new SqlParameter
            {
                ParameterName = key,
                Value = value,
                DbType = ConvertToDatabaseDataType(dataType)
            };
        }

        internal override DbConnection CreateDbConnection(string connectionString)
        {
            return new SqlConnection(connectionString);
        }
    }
}
