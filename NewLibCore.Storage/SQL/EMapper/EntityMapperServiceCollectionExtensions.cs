using System;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;
using NewLibCore.Logger;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.ProcessorFactory;
using NewLibCore.Storage.SQL.Template;

namespace NewLibCore.Storage.SQL.EMapper
{
    public static class EntityMapperExtensions
    {
        public static IServiceCollection AddEntityMapper(this IServiceCollection services, Action<EntityMapperOptions> entityMapperOptions)
        {
            services.Configure<EntityMapperOptions>(entityMapperOptions);
            var options = services.BuildServiceProvider().GetRequiredService<IOptions<EntityMapperOptions>>().Value;
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

            services = services.AddScoped<InsertComponent>();
            services = services.AddScoped<UpdateComponent>();
            services = services.AddScoped<SelectWrapper>();
            services = services.AddScoped<EntityMapper>();
            return services;
        }
    }
}