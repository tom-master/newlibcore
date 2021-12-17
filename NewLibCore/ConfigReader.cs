using System;
using Microsoft.Extensions.Configuration;
using NewLibCore.Validate;
using Com.Ctrip.Framework.Apollo;

namespace NewLibCore
{
    public interface IConfigReader
    {
        string Read(String key);
    }

    public class EnvironmentVariableReader: IConfigReader
    {
        public string Read(String key)
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
    }

    public class AppsettingsReader: IConfigReader
    {
        public string Read(string key)
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
    }
}
