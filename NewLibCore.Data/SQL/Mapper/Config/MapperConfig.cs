using System;
using System.Data;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Component.Cache;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Logger;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {
        /// <summary>
        /// 连接字符串名称
        /// </summary>
        /// <value></value>
        public static String ConnectionStringName { get; set; }

        /// <summary>
        /// 是否在出现异常时抛出异常
        /// </summary>
        /// <value></value>
        public static Boolean ThrowException { get; set; } = true;

        /// <summary>
        /// 启用模型验证
        /// </summary>
        /// <value></value>
        public static Boolean EnableModelValidate { get; set; }

        /// <summary>
        /// 事务隔离级别
        /// </summary>
        internal static IsolationLevel TransactionLevel { get; set; }

        /// <summary>
        /// 映射的数据库类型
        /// </summary>
        internal static MapperType MapperType { get; set; }

        /// <summary>
        /// mssql的版本
        /// </summary>
        internal static MsSqlPaginationVersion MsSqlPaginationVersion { get; set; } = MsSqlPaginationVersion.NONE;

        /// <summary>
        /// 初始化默认配置
        /// </summary>
        public static void InitDefaultSetting()
        {
            UseMySql();
            SetTransactionLevel(IsolationLevel.Unspecified);
            EnableModelValidate = true;
        }

        /// <summary>
        /// 切换为mysql
        /// </summary>
        public static void UseMySql()
        {
            MapperType = MapperType.MYSQL;
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        public static void UseMsSql()
        {
            MapperType = MapperType.MSSQL;
        }

        /// <summary>
        /// 设置事务隔离级别
        /// </summary>
        /// <param name="isolationLevel"></param>
        public static void SetTransactionLevel(IsolationLevel isolationLevel)
        {
            TransactionLevel = isolationLevel;
        }

        /// <summary>
        /// 设置自定义日志记录组件
        /// </summary>
        /// <param name="logger"></param>
        public static void SetLogger(ILogger logger)
        {
            Parameter.Validate(logger); 
        }

        /// <summary>
        /// 设置自定义查询缓存组件
        /// </summary>
        /// <param name="cache"></param>
        public static void SetCache(QueryCacheBase cache)
        {
            Parameter.Validate(cache); 
        }
    }
}
