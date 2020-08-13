using System;
using System.Configuration;
using Microsoft.Extensions.Configuration;
using NewLibCore.Validate;
using Com.Ctrip.Framework.Apollo;

namespace NewLibCore
{
    public static class Host
    {

        private static IConfigurationRoot ReadApollo()
        {
            var builder = new ConfigurationBuilder();
            var r = builder.AddJsonFile($"{Environment.CurrentDirectory}/appsettings.json").Build();
            return builder.AddApollo(r.GetSection("apollo")).AddDefault().Build();
            // return builder.AddConfiguration(r.GetSection("newcrm")).Build();
        }

        /// <summary>
        /// 获取环境变量
        /// </summary>
        /// <returns></returns>
        public static String GetHostVar(String varName)
        {
            Parameter.IfNullOrZero(varName);
            var v1 = Environment.GetEnvironmentVariable(varName, EnvironmentVariableTarget.Machine);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            v1 = Environment.GetEnvironmentVariable(varName, EnvironmentVariableTarget.Process);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            v1 = Environment.GetEnvironmentVariable(varName, EnvironmentVariableTarget.User);
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

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

            v1 = ReadApollo()[varName];
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            throw new Exception($@"没有找到设置的{varName}环境变量");
        }
    }
}
