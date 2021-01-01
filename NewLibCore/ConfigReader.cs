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
            var builder = new ConfigurationBuilder();
            var root = builder.AddApollo(builder.Build().GetSection("apollo")).AddDefault().Build();
            var value = root[key];
            if (string.IsNullOrEmpty(value))
            {
                return "";
            }
            return value;
        }

        private static String ReadFromEnvironmentVariable(String key)
        {
            Check.IfNullOrZero(key);
            var v1 = Environment.GetEnvironmentVariable(key, EnvironmentVariableTarget.Machine);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            v1 = Environment.GetEnvironmentVariable(key, EnvironmentVariableTarget.Process);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            v1 = Environment.GetEnvironmentVariable(key, EnvironmentVariableTarget.User);
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
            var value = root[key];
            if (string.IsNullOrEmpty(value))
            {
                return "";
            }
            return value;
        }

        /// <summary>
        /// 获取环境变量
        /// </summary>
        /// <returns></returns>
        public static String GetHostVar(String varName)
        {
            var v1 = "";
            v1 = ReadFromEnvironmentVariable(varName);
            if (!string.IsNullOrEmpty(v1))
            {
                return v1;
            }
            v1 = ReadFromAppsettings(varName);
            if (!string.IsNullOrEmpty(v1))
            {
                return v1;
            }
            v1 = ReadFromApollo(varName);
            if (!string.IsNullOrEmpty(v1))
            {
                return v1;
            }
            throw new ArgumentNullException($@"没有在环境变量,appsettings,apollo中找到对应的key:{varName}的值");
        }
    }
}
