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
        //private static MySqlConnection _connection;
        //private static readonly Object _sync = new Object();

        protected internal MySqlInstanceConfig(ILogger logger) : base(logger)
        {

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
            //if (_connection == null)
            //{
            //    lock (_sync)
            //    {
            //        if (_connection == null)
            //        {
            //            _connection = new MySqlConnection(ConnectionString);
            //        }
            //    }
            //}
            //return _connection;
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
                    Identity = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ",
                    RowCount = " ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c ",
                    Page = " LIMIT {value},{pageSize}"
                };
            }
        }
    }
}
