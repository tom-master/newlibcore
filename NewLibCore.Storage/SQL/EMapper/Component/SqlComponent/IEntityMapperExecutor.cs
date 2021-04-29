using System;

namespace NewLibCore.Storage.SQL.EMapper.Component.SqlComponent
{
    public interface IEntityMapperExecutor
    {
        String ComponentIdentity { get; }
        ExecutorResult Execute();
    }
}