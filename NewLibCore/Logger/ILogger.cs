using System;

namespace NewLibCore.Logger
{
    public interface ILogger
    {
        void Info(String message);

        void Error(String message);

        void Debug(String message);

        void Fail(String message);

        void Warn(String message);
    }
}
