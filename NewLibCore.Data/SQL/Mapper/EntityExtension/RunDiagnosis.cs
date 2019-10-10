using System;
using System.Diagnostics;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    internal sealed class RunDiagnosis
    {
        static RunDiagnosis()
        {
            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
        }

        private static void CurrentDomain_UnhandledException(Object sender, UnhandledExceptionEventArgs e)
        {
            MapperConfig.Logger.Error(((Exception)e.ExceptionObject).Message);
        }

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
            MapperConfig.Logger.Info($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 2)}s");
            return returnValue;
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