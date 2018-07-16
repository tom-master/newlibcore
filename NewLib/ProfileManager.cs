using System;
using System.Configuration;
using System.IO;
using NewLib.Security;

namespace NewLib
{
    public class ProfileManager
    {
        private static String _profileName;

        public static void Init(String profileName = default(String))
        {
            _profileName = profileName;
            var watcher = new FileSystemWatcher
            {
                Path = AppDomain.CurrentDomain.BaseDirectory,
                Filter = profileName ?? "WebSite.config"
            };

            watcher.Changed += (obj, sender) =>
            {
                Reload();
            };

            Reload();
            watcher.EnableRaisingEvents = true;
        }

        private static void Reload()
        {
            var config = ConfigurationManager.OpenMappedExeConfiguration(new ExeConfigurationFileMap
            {
                ExeConfigFilename = $@"{AppDomain.CurrentDomain.BaseDirectory}/{(_profileName ?? "WebSite.config")}"
            }, ConfigurationUserLevel.None);
            FileUrl = SensitiveDataSafetyProvider.Decrypt(config.AppSettings.Settings["FileUrl"].Value);
            RedisConnection = SensitiveDataSafetyProvider.Decrypt(config.AppSettings.Settings["RedisConnection"].Value);
            RedisPrefix = SensitiveDataSafetyProvider.Decrypt(config.AppSettings.Settings["RedisPrefix"].Value);
            RedisDbNum = Int32.Parse(config.AppSettings.Settings["RedisDb"].Value);
            UploadUrl = config.AppSettings.Settings["UploadUrl"].Value;
        }

        public static String FileUrl { get; private set; }

        public static String RedisConnection { get; private set; }

        public static String RedisPrefix { get; private set; }

        public static Int32 RedisDbNum { get; private set; }

        public static String UploadUrl { get; private set; }
    }
}
