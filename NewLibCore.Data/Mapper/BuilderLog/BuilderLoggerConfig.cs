using System;

namespace NewLibCore.Data.Mapper.BuilderLog
{
	public class BuilderLoggerConfig
	{
		/// <summary>
		/// 日志文件名
		/// </summary>
		public String LogFileName { get; set; }

		/// <summary>
		/// 日志文件大小（MB）
		/// </summary>
		public Int32 LogFileCapacity { get; set; }

		/// <summary>
		/// 日志文件根路径
		/// </summary>
		public String LogFileRootDirectory
		{
			get
			{
				var logFileRootDirectory = Environment.GetEnvironmentVariable("LogFileRootDirectory");
				if (!String.IsNullOrEmpty(logFileRootDirectory))
				{
					return logFileRootDirectory;
				}
				throw new ArgumentException("没有配置日志文件的根路径");
			}
		}
	}
}
