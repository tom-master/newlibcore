﻿using System;
using System.Data.Common;
using System.Data.SqlClient;
using NewLibCore.Data.SQL.Mapper.Database;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// mssql数据库实例配置
    /// </summary>
    internal class MsSqlInstanceConfig : InstanceConfig
    {
        /// <summary>
        /// 初始化一个MsSqlInstanceConfig类的实例
        /// </summary>
        public MsSqlInstanceConfig(DbContext dbContext)
        {
            DbContext = dbContext;
        }


        internal override InstanceExtension Extension
        {
            get
            {
                return new InstanceExtension
                {
                    Identity = " SELECT @@IDENTITY ;",
                    RowCount = " SELECT @@ROWCOUNT ;",
                    Page = " OFFSET ({value}) ROWS FETCH NEXT {pageSize} ROWS ONLY ;"
                };
            }
        }

        internal override String UpdateTemplate
        {
            get
            {
                return "UPDATE {0} SET {1} FROM {2} AS {0} ";
            }
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

        internal override String RelationBuilder(RelationType relationType, String left, String right)
        {
            return String.Format(RelationMapper[relationType], left, right);
        }
    }
}
