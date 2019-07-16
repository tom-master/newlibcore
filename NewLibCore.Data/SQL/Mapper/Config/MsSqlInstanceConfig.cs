using System;
using System.Data.Common;
using System.Data.SqlClient;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// mssql数据库实例配置
    /// </summary>
    internal class MsSqlInstanceConfig : DatabaseInstanceConfig
    {
        protected internal MsSqlInstanceConfig(ILogger logger) : base(logger)
        {

        }

        protected override void AppendRelationType()
        {
            RelationMapper.Add(RelationType.FULL_LIKE, "{0} LIKE '%{1}%'");
            RelationMapper.Add(RelationType.START_LIKE, "{0} LIKE '{1}%'");
            RelationMapper.Add(RelationType.END_LIKE, "{0} LIKE '%{1}' ");
            RelationMapper.Add(RelationType.IN, "{0} IN ({1})");
        }

        internal override DbConnection GetConnectionInstance()
        {
            return new SqlConnection(ConnectionString);
        }

        internal override DbParameter GetParameterInstance()
        {
            return new SqlParameter();
        }

        internal override String RelationBuilder(RelationType relationType, String left, Object right)
        {
            return String.Format(RelationMapper[relationType], left, right);
        }

        internal override InstanceExtension Extension
        {
            get
            {
                return new InstanceExtension
                {
                    Identity = " SELECT @@IDENTITY",
                    RowCount = " SELECT @@ROWCOUNT",
                    Page = " OFFSET ({value}) ROWS FETCH NEXT {pageSize} ROWS ONLY"
                };
            }
        }
    }
}
