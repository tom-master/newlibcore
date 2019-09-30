using System;
using System.Data;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {

        /// <summary>
        /// 映射的数据库类型
        /// </summary>
        public static MapperType MapperType { get; set; } = MapperType.MYSQL;

        /// <summary>
        /// 日志
        /// </summary>
        public static ILogger Logger { get; } = new ConsoleLogger();

        /// <summary>
        /// 启用模型验证
        /// </summary>
        /// <value></value>
        public static Boolean EnableModelValidate { get; set; } = true;

        /// <summary>
        /// 事务隔离级别
        /// </summary>
        public static IsolationLevel TransactionLevel { get; set; } = IsolationLevel.Unspecified;


        internal static InstanceConfig Instance
        {
            get
            {
                if (MapperType == MapperType.MSSQL)
                {
                    return new MsSqlInstanceConfig();
                }
                else if (MapperType == MapperType.MYSQL)
                {
                    return new MySqlInstanceConfig();
                }
                throw new Exception($@"暂不支持的数据库类型:{MapperType}");
            }
        }
    }
}
