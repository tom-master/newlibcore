using System;
using System.Data.Common;
using System.Text;
using MySql.Data.MySqlClient;
using NewLibCore.Storage.SQL.Component;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Template
{
    /// <summary>
    /// mysql数据库sql模板配置
    /// </summary>
    internal class MySqlTemplate : TemplateBase
    {
        public MySqlTemplate() : base() { }
        internal override StringBuilder CreateUpdate(string tableName, string aliasName, string placeHolders)
        {
            var s = $@"UPDATE {tableName} AS {aliasName} SET {placeHolders} <where>";
            return new StringBuilder(s);
        }

        protected override void AppendPredicateType()
        {
            PredicateMapper.Add(EMType.FULL_LIKE, "{0} LIKE CONCAT('%',{1},'%')");
            PredicateMapper.Add(EMType.START_LIKE, "{0} LIKE CONCAT('',{1},'%')");
            PredicateMapper.Add(EMType.END_LIKE, "{0} LIKE CONCAT('%',{1},'')");
            PredicateMapper.Add(EMType.IN, "FIND_IN_SET({0},{1})");
        }

        internal override string CreatePredicate(EMType predicateType, string left, string right)
        {
            Check.IfNullOrZero(predicateType);
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(right);

            return string.Format(PredicateMapper[predicateType], left, right);
        }

        internal override void CreatePagination(PaginationComponent pagination, string orderBy, StringBuilder rawSql)
        {
            Check.IfNullOrZero(pagination.Size);
            Check.IfNullOrZero(orderBy);
            Check.IfNullOrZero(rawSql);
            rawSql = rawSql.Append($@" {orderBy} LIMIT {pagination.Size * (pagination.Index - 1)},{pagination.Size} ;");
        }

        internal override DbParameter CreateParameter(string key, Object value, Type dataType)
        {
            return new MySqlParameter
            {
                ParameterName = key,
                Value = value,
                DbType = ConvertToDatabaseDataType(dataType)
            };
        }

        internal override DbConnection CreateDbConnection(string connectionString)
        {
            return new MySqlConnection(connectionString);
        }

        internal override string Identity
        {
            get
            {
                return "; SELECT CAST(@@IDENTITY AS SIGNED) ;";
            }
        }

        internal override string AffectedRows
        {
            get
            {
                return "; SELECT CAST(ROW_COUNT() AS SIGNED) ;";
            }
        }
    }
}
