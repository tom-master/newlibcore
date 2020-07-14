using System;
using System.Diagnostics;

namespace NewLibCore.Logger
{
    public class DefaultLogger : ILogger
    {
        /// <summary>
        /// 输出调试日志
        /// </summary>
        /// <param name="message"></param>
        public void Debug(String message)
        {
            Write(LoggerLevel.Debug, message);
        }

        /// <summary>
        /// 输出错误日志
        /// </summary>
        /// <param name="message"></param>
        public void Error(String message)
        {
            Write(LoggerLevel.Exception, message);
        }

        /// <summary>
        /// 输出失败日志
        /// </summary>
        /// <param name="message"></param>
        public void Fail(String message)
        {
            Write(LoggerLevel.Error, message);
        }

        /// <summary>
        /// 输出一般性信息日志
        /// </summary>
        /// <param name="message"></param>
        public void Info(String message)
        {
            Write(LoggerLevel.Info, message);
        }

        /// <summary>
        /// 输出警告信息
        /// </summary>
        /// <param name="message"></param>
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
