using System;
using Microsoft.Extensions.DependencyInjection;

namespace NewLibCore.Storage.SQL.EMapper
{
    public static class EntityMapperExtensions
    {
        public static IServiceCollection AddEntityMapper(this IServiceCollection services, Action<EntityMapperOptions> options)
        {
            services.Configure(options);
            var entityMapperBuilder = new EntityMapperBuilder(services);
            services.AddSingleton(entityMapperBuilder);
            return services;
        }
    }
}