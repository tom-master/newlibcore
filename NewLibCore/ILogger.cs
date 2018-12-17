using System;

namespace NewLibCore
{
    public interface ILogger
    {
        void Write(String level, String message);
    }
}
