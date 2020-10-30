using System;
using System.Diagnostics;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Logger;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Extension
{
    internal static class RunDiagnosis
    {
        private static ILogger _logger;

        static RunDiagnosis()
        {
            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
        }

        private static void CurrentDomain_UnhandledException(Object sender, UnhandledExceptionEventArgs e)
        {
            _logger.Error(((Exception)e.ExceptionObject).Message);
            if (EntityMapperConfig.ThrowException)
            {
                throw (Exception)e.ExceptionObject;
            }
        }

        /// <summary>
        /// 设置日志组件实例
        /// </summary>
        /// <param name="logger"></param>
        internal static void SetLoggerInstance(ILogger logger)
        {
            Check.IfNullOrZero(logger);
            _logger = logger;
        }

        /// <summary>
        /// 输出从表达式解析到sql语句执行完成的时间
        /// </summary>
        /// <param name="func"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        internal static TModel Watch<TModel>(Func<TModel> func)
        {
            try
            {
                Check.IfNullOrZero(func);
                var sw = new Stopwatch();
                sw.Start();
                var returnValue = func();
                sw.Stop();
                _logger.Info($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 4)}秒");
                return returnValue;
            }
            catch (Exception)
            {
                throw;
            }
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