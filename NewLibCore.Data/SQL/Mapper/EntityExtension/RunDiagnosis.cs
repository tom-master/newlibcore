using System;
using System.Diagnostics;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    internal sealed class RunDiagnosis
    {
        private static ILogger _logger = MapperConfig.Logger ?? new ConsoleLogger();

        /// <summary>
        /// 输出从表达式解析到sql语句执行完成的时间
        /// </summary>
        /// <param name="func"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        internal static TModel Watch<TModel>(Func<TModel> func)
        {
            var sw = new Stopwatch();
            sw.Start();
            var returnValue = func();
            sw.Stop();
            _logger.Info($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 2)}s");
            return returnValue;
        }

        internal static void Info(String message)
        {
            _logger.Info(message);
        }

        internal static void Error(String message)
        {
            _logger.Error(message);
        }

        internal static void Debug(String message)
        {
            _logger.Debug(message);
        }

        internal static void Fail(String message)
        {
            _logger.Fail(message);
        }

        internal static void Warn(String message)
        {
            _logger.Warn(message);
        }
    }
}