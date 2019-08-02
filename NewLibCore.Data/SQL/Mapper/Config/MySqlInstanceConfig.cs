using System;
using System.Data.Common;
using MySql.Data.MySqlClient;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// mysql数据库实例配置
    /// </summary>
    internal class MySqlInstanceConfig : InstanceConfig
    {
        protected internal MySqlInstanceConfig(ILogger logger) : base(logger)
        {

        }

        internal override String UpdateTemplate
        {
            get
            {
                return "UPDATE {0} AS {1} SET {2} ";
            }
        }

        protected override void AppendRelationType()
        {
            RelationMapper.Add(RelationType.FULL_LIKE, "{0} LIKE CONCAT('%',{1},'%')");
            RelationMapper.Add(RelationType.START_LIKE, "{0} LIKE CONCAT('',{1},'%')");
            RelationMapper.Add(RelationType.END_LIKE, "{0} LIKE CONCAT('%',{1},'')");
            RelationMapper.Add(RelationType.IN, "FIND_IN_SET({0},{1})");
        }

        internal override DbConnection GetConnectionInstance()
        {
            return new MySqlConnection(ConnectionString);
        }

        internal override DbParameter GetParameterInstance()
        {
            return new MySqlParameter();
        }

        internal override String RelationBuilder(RelationType relationType, String left, String right)
        {
            return String.Format(RelationMapper[relationType], left, right);
        }

        internal override InstanceExtension Extension
        {
            get
            {
                return new InstanceExtension
                {
                    Identity = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ;",
                    RowCount = " ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c ;",
                    Page = " LIMIT {value},{pageSize} ;",
                };
            }
        }
    }
}
