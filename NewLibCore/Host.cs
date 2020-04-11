﻿using System;
using System.Configuration;
using Microsoft.Extensions.Configuration;
using NewLibCore.Validate;
using Com.Ctrip.Framework.Apollo;

namespace NewLibCore
{
    public static class Host
    {
        private static readonly IConfigurationRoot _configRoot;

        static Host()
        {
            var builder = new ConfigurationBuilder();
            var r = builder.AddJsonFile($"{Environment.CurrentDirectory}/appsettings.json").Build();
            builder.AddApollo(r.GetSection("apollo")).AddDefault();
            _configRoot = builder.Build();
        }

        /// <summary>
        /// 获取环境变量
        /// </summary>
        /// <returns></returns>
        public static String GetHostVar(String varName)
        {
            Parameter.Validate(varName);

            if (String.IsNullOrEmpty(varName))
            {
                throw new ArgumentException("varName不能为空");
            }

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

            v1 = _configRoot[varName];
            if (!String.IsNullOrEmpty(v1))
            {
                return v1;
            }

            throw new Exception($@"没有找到设置的{varName}环境变量");
        }
    }
}
