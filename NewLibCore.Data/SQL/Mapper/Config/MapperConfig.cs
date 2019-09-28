using System;
using System.Data;
using Microsoft.Extensions.DependencyInjection;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {

        public static MapperType MapperType { get; set; } = MapperType.MYSQL;

        /// <summary>
        /// 启用模型验证
        /// </summary>
        /// <value></value>
        public static Boolean EnableModelValidate { get; set; } = true;

        /// <summary>
        /// 事务隔离级别
        /// </summary>
        public static IsolationLevel TransactionLevel { get; set; } = IsolationLevel.Unspecified;

        /// <summary>
        /// 提供依赖注入的对象
        /// </summary>
        internal static IServiceProvider ServiceProvider { get; set; }
    }
}
