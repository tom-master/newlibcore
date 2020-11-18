using System;

namespace NewLibCore.Storage.SQL.Component
{
    public interface IEntityMapperExecutor
    {
        String ComponentIdentity { get; }
        ExecutorResult Execute();
    }
}