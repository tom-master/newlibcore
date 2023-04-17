using System;

namespace NewLibCore.Storage.SQL.Component
{
    public interface IEntityMapperExecutor
    {
        string ComponentIdentity { get; }
        ExecutorResult Execute();
    }
}