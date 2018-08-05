using System;
using NewLibCore;
namespace NewLibCore.Data.Mapper.BuilderLog
{
	public class BuilderLoggerConfig
	{
		/// <summary>
		/// 日志文件名
		/// </summary>
		public String LogFileName { get; set; } = DateTime.Now.ToString("yyyy-MM-dd") + ".log";

		/// <summary>
		/// 日志文件大小（MB）
		/// </summary>
		public Int32 LogFileCapacity { get; set; } = 5;

		/// <summary>
		/// 存储日志的文件夹
		/// </summary>
		public String LoggerFileFolder { get; set; } = "logs";

		/// <summary>
		/// 是否以一定的大小生成新日志文件
		/// </summary>
		public Boolean IsAutoCreateLoggerFileByCapacity { get; set; } = false;

		/// <summary>
		/// 是否以一定的时间段生成新日志文件
		/// </summary>
		public TimeSpan IsAutoCreateLoggerFileByTimeSpan { get; set; } = new TimeSpan();

		/// <summary>
		/// 清理一个日期之前的日志文件
		/// </summary>
		public DateTime ClearOldLoggerFile { get; set; } = new DateTime();

		/// <summary>
		/// 日志文件根路径
		/// </summary>
		public String LogFileRootDirectory
		{
			get
			{
				return NewLibCore.Host.GetHostVar("LogFileRootDirectory");
			}
		}
	}

	/// <summary>
	/// 日志写入模式
	/// </summary>
	public enum LoggerWriteMode
	{

	}
}
