using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Storage.SQL.Component;

namespace NewLibCore.Storage.SQL
{
    public static class EntityMapperExtensions
    {
        public static IServiceCollection AddEntityMapper(this IServiceCollection services, Action<EntityMapperOptions> entityMapperOptions)
        {
            services.Configure(entityMapperOptions);

            services.AddScoped<ResultExecutor>();
            services.AddScoped<EntityMapper>();
            return services;
        }

        public static IServiceCollection AddConfigReader<TConfigReader>(this IServiceCollection services, TConfigReader configReader) where TConfigReader : IConfigReader
        {
            services.AddSingleton<IConfigReader>(configReader);
            return services;
        }

        public static IServiceCollection AddEnvironmentVariableReader(this IServiceCollection services)
        {
            services.AddSingleton<IConfigReader, EnvironmentVariableReader>();
            return services;
        }

        public static IServiceCollection AddAppsettingsReader(this IServiceCollection services)
        {
            services.AddSingleton<IConfigReader, AppsettingsReader>();
            return services;
        }
    }
}