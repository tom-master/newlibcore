using System;
using System.Data.Common;
using System.Data.SqlClient;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// mssql数据库sql模板配置
    /// </summary>
    internal class MsSqlInstanceConfig : InstanceConfig
    {
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

        protected override void AppendPredicateType()
        {
            PredicateMapper.Add(PredicateType.FULL_LIKE, "{0} LIKE '%{1}%'");
            PredicateMapper.Add(PredicateType.START_LIKE, "{0} LIKE '{1}%'");
            PredicateMapper.Add(PredicateType.END_LIKE, "{0} LIKE '%{1}' ");
            PredicateMapper.Add(PredicateType.IN, "{0} IN ({1})");
        }

        internal override String CreatePredicate(PredicateType predicateType, String left, String right)
        {
            return String.Format(PredicateMapper[predicateType], left, right);
        }
    }
}
