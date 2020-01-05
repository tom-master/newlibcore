using System;
using System.Diagnostics;
using NewLibCore.Logger;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    internal class RunDiagnosis
    {
        private ILogger _logger;

        public RunDiagnosis(ILogger logger)
        {
            _logger = logger;
            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
        }

        private void CurrentDomain_UnhandledException(Object sender, UnhandledExceptionEventArgs e)
        {
            _logger.Error(((Exception)e.ExceptionObject).Message);
            if (EntityMapper.ThrowException)
            {
                throw (Exception)e.ExceptionObject;
            }
        }

        /// <summary>
        /// 输出从表达式解析到sql语句执行完成的时间
        /// </summary>
        /// <param name="func"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        internal TModel Watch<TModel>(Func<TModel> func)
        {
            try
            {
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

        internal void Info(String message)
        {
            _logger.Info(message);
        }

        internal void Error(String message)
        {
            _logger.Error(message);
        }

        internal void Debug(String message)
        {
            _logger.Debug(message);
        }

        internal void Fail(String message)
        {
            _logger.Fail(message);
        }

        internal void Warn(String message)
        {
            _logger.Warn(message);
        }
    }
}