using System;
using System.Diagnostics;

namespace NewLibCore.Logger
{
    public class DefaultLogger : ILogger
    {
        public void Debug(String message)
        {
            Write(LoggerLevel.Debug, message);
        }

        public void Error(String message)
        {
            Write(LoggerLevel.Exception, message);
        }

        public void Fail(String message)
        {
            Write(LoggerLevel.Error, message);
        }

        public void Info(String message)
        {
            Write(LoggerLevel.Info, message);
        }

        public void Warn(String message)
        {
            Write(LoggerLevel.Warning, message);
        }

        private void Write(LoggerLevel level, String message)
        {
            ConsoleColor consoleColor = default;
            switch (level)
            {
                case LoggerLevel.Info:
                    consoleColor = ConsoleColor.White;
                    break;
                case LoggerLevel.Warning:
                    consoleColor = ConsoleColor.Yellow;
                    break;
                case LoggerLevel.Debug:
                    consoleColor = ConsoleColor.Cyan;
                    break;
                case LoggerLevel.Error:
                case LoggerLevel.Exception:
                    consoleColor = ConsoleColor.Red;
                    break;
                default:
                    break;
            }
            var stackTrace = new StackTrace();
            var stackFrame = stackTrace.GetFrame(2);
            var type = stackFrame.GetMethod().DeclaringType;
            Console.ForegroundColor = consoleColor;
            Console.WriteLine($@"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}][{level}][{type.Name}]:{message}");
        }
    }
}
