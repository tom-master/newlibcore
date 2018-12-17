using System;

namespace NewLibCore
{
    public class ConsoleLogger : ILogger
    {
        public Object _type;

        public ConsoleLogger(Object type)
        {
            _type = type;
        }

        public void Write(String level, String message)
        {
            Console.WriteLine($@"[{DateTime.Now:yyyy-MM-dd}][{level.ToUpper()}][{_type.GetType().Name}]:{message}");
        }
    }
}
