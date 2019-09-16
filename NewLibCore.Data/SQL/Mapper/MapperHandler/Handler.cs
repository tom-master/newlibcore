using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 表达式处理基类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal abstract class Handler
    {
        /// <summary>
        /// 初始化一个Handler类的实例
        /// </summary>
        internal Handler()
        {
            Instance = MapperConfig.ServiceProvider.GetService<InstanceConfig>();
        }

        /// <summary>
        /// 数据库配置实例
        /// </summary>
        protected InstanceConfig Instance { get; private set; }

        protected DbContext DbContext { get; private set; }

        /// <summary>
        /// 执行表达式段的翻译
        /// </summary>
        /// <returns></returns>
        internal abstract RawExecuteResult Execute();

        /// <summary>
        /// 设置数据库访问上下文
        /// </summary>
        /// <param name="dbContext"></param>
        internal void AddDbContext(DbContext dbContext)
        {
            Parameter.Validate(dbContext);
            DbContext = dbContext;
        }

        internal virtual void AddSegmentManager(SegmentManager segmentManager)
        {

        }
    }
}
