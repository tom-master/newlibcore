using System;
using System.Diagnostics;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    internal sealed class RunDiagnosis
    {
        static RunDiagnosis()
        {
            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
        }

        private static void CurrentDomain_UnhandledException(Object sender, UnhandledExceptionEventArgs e)
        {
            Error(((Exception)e.ExceptionObject).Message);
            if (MapperConfig.ThrowException)
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
        internal static TModel Watch<TModel>(Func<TModel> func)
        {
            try
            {
                var sw = new Stopwatch();
                sw.Start();
                var returnValue = func();
                sw.Stop();
                Info($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 4)}秒");
                return returnValue;
            }
            catch (Exception)
            {
                throw;
            }
        }

        internal static void Info(String message)
        {
            MapperConfig.Logger.Info(message);
        }

        internal static void Error(String message)
        {
            MapperConfig.Logger.Error(message);
        }

        internal static void Debug(String message)
        {
            MapperConfig.Logger.Debug(message);
        }

        internal static void Fail(String message)
        {
            MapperConfig.Logger.Fail(message);
        }

        internal static void Warn(String message)
        {
            MapperConfig.Logger.Warn(message);
        }
    }
}