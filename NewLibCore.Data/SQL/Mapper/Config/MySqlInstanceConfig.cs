﻿using System;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// mysql数据库实例配置
    /// </summary>
    internal class MySqlInstanceConfig : InstanceConfig
    {
        internal override String UpdateTemplate
        {
            get
            {
                return "UPDATE {0} AS {1} SET {2} ";
            }
        }

        protected override void AppendRelationType()
        {
            LogicRelationMapper.Add(RelationType.FULL_LIKE, "{0} LIKE CONCAT('%',{1},'%')");
            LogicRelationMapper.Add(RelationType.START_LIKE, "{0} LIKE CONCAT('',{1},'%')");
            LogicRelationMapper.Add(RelationType.END_LIKE, "{0} LIKE CONCAT('%',{1},'')");
            LogicRelationMapper.Add(RelationType.IN, "FIND_IN_SET({0},{1})");
        }

        internal override String RelationBuilder(RelationType relationType, String left, String right)
        {
            return String.Format(LogicRelationMapper[relationType], left, right);
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
