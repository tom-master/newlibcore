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
        public static MapperType MapperType { get; set; }

        /// <summary>
        /// 日志
        /// </summary>
        public static ILogger Logger { get; set; }

        /// <summary>
        /// 启用模型验证
        /// </summary>
        /// <value></value>
        public static Boolean EnableModelValidate { get; set; }

        /// <summary>
        /// 事务隔离级别
        /// </summary>
        public static IsolationLevel TransactionLevel { get; set; }

        /// <summary>
        /// 初始化默认配置
        /// </summary>
        public static void InitDefaultSetting()
        {
            MapperType = MapperType.MYSQL;
            Logger = new ConsoleLogger();
            EnableModelValidate = true;
            TransactionLevel = IsolationLevel.Unspecified;
        }
    }
}
