using System;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// mysql数据库sql模板配置
    /// </summary>
    internal class MySqlTemplate : TemplateBase
    {
        internal override String UpdateTemplate
        {
            get
            {
                return "UPDATE {0} AS {1} SET {2} ";
            }
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
            return String.Format(PredicateMapper[predicateType], left, right);
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
