using System;
using System.Data.Common;
using Microsoft.Extensions.Options;
using MySql.Data.MySqlClient;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Store;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Template
{
    /// <summary>
    /// mysql数据库sql模板配置
    /// </summary>
    internal class MySqlTemplate : TemplateBase
    {
        public MySqlTemplate(IOptions<EntityMapperOptions> options) : base(options) { }
        internal override String CreateUpdate<TModel>(TModel model)
        {
            var (tableName, aliasName) = model.GetEntityBaseAliasName();
            return $@"UPDATE {tableName} AS {aliasName} SET {model.GetSqlElements().UpdatePlaceHolders}";
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
            Check.IfNullOrZero(predicateType);
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(right);

            return String.Format(PredicateMapper[predicateType], left, right);
        }

        internal override String CreatePagination(PaginationComponent pagination, String orderBy, String rawSql)
        {
            Check.IfNullOrZero(pagination.Size);
            Check.IfNullOrZero(orderBy);
            Check.IfNullOrZero(rawSql);

            if (pagination.MaxKey > 0)
            {
                return $@"{rawSql} AND {pagination.QueryMainTable.Value}.{PrimaryKeyName}<{pagination.MaxKey} {orderBy} LIMIT {pagination.Size} ;";
            }

            return $@"{rawSql} {orderBy} LIMIT {pagination.Size * (pagination.Index - 1)},{pagination.Size} ;";
        }

        internal override DbParameter CreateParameter(String key, Object value, Type dataType)
        {
            return new MySqlParameter
            {
                ParameterName = key,
                Value = value,
                DbType = ConvertToDatabaseDataType(dataType)
            };
        }

        internal override DbConnection CreateDbConnection()
        {
            return new MySqlConnection(ConfigReader.GetHostVar(Options.ConnectionStringName));
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
