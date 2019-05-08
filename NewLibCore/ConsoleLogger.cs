using System;
using System.Diagnostics;

namespace NewLibCore
{
    public class ConsoleLogger : ILogger
    {
        public ConsoleLogger()
        {
        }

        public void Write(String level, String message)
        {
            var stackTrace = new StackTrace();
            var stackFrame = stackTrace.GetFrame(1);
            var type = stackFrame.GetMethod().DeclaringType;
            Console.WriteLine($@"[{DateTime.Now:yyyy-MM-dd}][{level.ToUpper()}][{type.Name}]:{message}");
        }
    }
}
