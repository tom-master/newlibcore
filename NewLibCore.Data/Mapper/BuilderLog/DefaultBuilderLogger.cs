using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace NewLibCore.Data.Mapper.BuilderLog
{
	internal class DefaultBuilderLogger : IBuilderLogger
	{
		private readonly static ReaderWriterLockSlim _readerWriterLock = new ReaderWriterLockSlim();

		private readonly BuilderLoggerConfig _config;

		internal DefaultBuilderLogger(BuilderLoggerConfig config = null)
		{
			_config = config ?? BuilderFactory.GetDefaultLoggerConfig();
		}

		public Task WriteLogAsync(Object model)
		{
			return Task.Run(() =>
			{
				try
				{
					_readerWriterLock.EnterWriteLock();

					if (_config.IsAutoCreateLoggerFileByCapacity)
					{

					}
					var loggerPath = $@"{_config.LogFileRootDirectory}\{_config.LoggerFileFolder}\{_config.LogFileName}";
					using (var stream = new FileStream(loggerPath, FileMode.Create, FileAccess.Write))
					{

					}
				}
				finally
				{
					_readerWriterLock.ExitWriteLock();
				}
			});
		}
	}
}

