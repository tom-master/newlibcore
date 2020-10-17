using System;
using System.Configuration;
using Microsoft.Extensions.Configuration;
using NewLibCore.Validate;
using Com.Ctrip.Framework.Apollo;

namespace NewLibCore
{
    public static class ConfigReader
    {

        private static String ReadFromApollo(String key)
        {
            return "";
        }

        private static String ReadFromEnvironmentVariable(String varKey)
        {
            Check.IfNullOrZero(varKey);
            var v1 = Environment.GetEnvironmentVariable(varKey, EnvironmentVariableTarget.Machine);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            v1 = Environment.GetEnvironmentVariable(varKey, EnvironmentVariableTarget.Process);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            v1 = Environment.GetEnvironmentVariable(varKey, EnvironmentVariableTarget.User);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }
            return "";
        }

        private static String ReadFromAppsettings(String key)
        {
            Check.IfNullOrZero(key);
            var builder = new ConfigurationBuilder();
            var root = builder.AddJsonFile($@"{AppDomain.CurrentDomain.BaseDirectory}/appsettings.json").Build();
            var path = $@"{key}";
            var result = root[path];
            if (!string.IsNullOrEmpty(result))
            {
                return result;
            }
            throw new ArgumentException($@"没有找到{path}对应的值");
        }

        /// <summary>
        /// 获取环境变量
        /// </summary>
        /// <returns></returns>
        public static String GetHostVar(String varName)
        {
            var v1 = "";
            v1 = ConfigurationManager.AppSettings[varName];
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            v1 = ConfigurationManager.ConnectionStrings[varName]?.ConnectionString;
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            throw new Exception($@"没有找到设置的{varName}环境变量");
        }
    }
}
