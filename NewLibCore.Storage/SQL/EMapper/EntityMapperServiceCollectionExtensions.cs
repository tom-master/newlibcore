using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Storage.SQL.Component;

namespace NewLibCore.Storage.SQL
{
    public static class EntityMapperExtensions
    {
        public static IServiceCollection AddEntityMapper(this IServiceCollection services, Action<EntityMapperOptions> entityMapperOptions)
        {
            services.Configure<EntityMapperOptions>(entityMapperOptions);
            services.AddScoped<IEntityMapperExecutor, InsertComponent>();
            services.AddScoped<IEntityMapperExecutor, UpdateComponent>();
            services.AddScoped<IEntityMapperExecutor, SelectComponent>();
            services.AddScoped<MapperDbContextBase, MapperDbContext>();
            services.AddScoped<ResultExecutor>();
            services.AddScoped<EntityMapper>();
            return services;
        }
    }
}