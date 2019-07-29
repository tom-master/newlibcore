using System;
using System.Configuration;
using NewLibCore.Validate;

namespace NewLibCore
{
    public static class Host
    {
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
            
            throw new Exception($@"没有找到设置的{varName}环境变量");
        }
    }
}
