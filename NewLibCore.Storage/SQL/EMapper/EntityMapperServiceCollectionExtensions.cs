using System;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;
using NewLibCore.Logger;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper.Component.SqlComponent;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.EMapper
{
    public static class EntityMapperExtensions
    {
        public static IServiceCollection AddEntityMapper(this IServiceCollection services, Action<EntityMapperOptions> entityMapperOptions)
        {
            services.Configure<EntityMapperOptions>(entityMapperOptions);
            services.AddScoped<IEntityMapperExecutor, InsertComponent>();
            services.AddScoped<IEntityMapperExecutor, UpdateComponent>();
            services.AddScoped<IEntityMapperExecutor, SelectComponent>();
            services = services.AddScoped<EntityMapper>();
            return services;
        }
    }
}