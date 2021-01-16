using System;
using Microsoft.Extensions.DependencyInjection;

namespace NewLibCore.Storage.SQL.EMapper
{
    public static class EntityMapperExtensions
    {
        public static EntityMapperBuilder AddEntityMapper(this IServiceCollection services)
        {
            return new EntityMapperBuilder();
        }
    }
}