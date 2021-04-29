using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.EMapper.Component.SqlComponent;

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
            services.AddScoped<MapperDbContextBase, MapperDbContext>();
            services.AddScoped<PredicateExpressionTranslatorResultExecutor>();
            services.AddScoped<EntityMapper>();
            return services;
        }
    }
}