using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.ProcessorFactory;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Logger;

namespace NewLibCore.Storage.SQL.EMapper
{
    internal class EntityMapperBuilder
    {
        internal IServiceProvider ServiceProvider { get; private set; }
        /// <summary>
        /// 初始化依赖注入
        /// </summary>
        internal EntityMapperBuilder(IServiceCollection services)
        {
            var options = services.BuildServiceProvider().GetRequiredService<EntityMapperOptions>();
            if (options.MapperType == MapperType.NONE)
            {
                throw new Exception("没有指定要映射的数据库类型");
            }
            if (options.MapperType == MapperType.MSSQL)
            {
                services = services.AddScoped<TemplateBase, MsSqlTemplate>();
            }
            else if (options.MapperType == MapperType.MYSQL)
            {
                services = services.AddScoped<TemplateBase, MySqlTemplate>();
            }

            services = services.AddScoped<MapperDbContextBase, MapperDbContext>();
            RunDiagnosis.SetLoggerInstance(options.Logger ?? new DefaultLogger());

            services = services.AddTransient<ConditionProcessor, DefaultConditionProcessor>();
            services = services.AddTransient<ProcessorResult>();

            services = services.AddTransient<Processor, RawSqlProcessor>();
            services = services.AddTransient<Processor, QueryProcessor>();
            services = services.AddTransient<Processor, UpdateProcessor>();
            services = services.AddTransient<Processor, InsertProcessor>();

            ServiceProvider = services.BuildServiceProvider();
        }
    }
}
