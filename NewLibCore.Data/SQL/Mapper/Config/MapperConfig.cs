﻿using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 映射配置
    /// </summary>
    public class MapperConfig
    {
        private static readonly Object _obj = new Object();

        private MapperConfig()
        {

        }

        public static MapperConfig CreateMapperConfig()
        {
            return new MapperConfig();
        }

        internal static ILogger Logger { get; private set; }

        internal static InstanceConfig Instance { get; private set; }

        /// <summary>
        /// 设置日志
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public static void SetLogger(ILogger logger)
        {
            Logger = logger;
        }

        /// <summary>
        /// 切换为mysql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public void SwitchToMySql(Boolean cache = false)
        {
            SwitchTo(DatabaseType.MYSQL, cache);
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        /// <param name="logger"></param>
        /// <returns></returns>
        public void SwitchToMsSql(Boolean cache = false)
        {
            SwitchTo(DatabaseType.MSSQL, cache);
        }

        /// <summary>
        /// 切换到指定数据库配置实例
        /// </summary>
        /// <param name="database"></param>
        /// <param name="logger"></param>
        private static void SwitchTo(DatabaseType database, Boolean cache)
        {
            switch (database)
            {
                case DatabaseType.MSSQL:
                {
                    Instance = new MsSqlInstanceConfig();
                    break;
                }
                case DatabaseType.MYSQL:
                {
                    Instance = new MySqlInstanceConfig();
                    break;
                }
                default:
                {
                    throw new ArgumentException($@"暂不支持的数据库类型:{database}");
                }
            }
            if (cache)
            {
                Instance.UseCache();
            }
        }
    }
}
