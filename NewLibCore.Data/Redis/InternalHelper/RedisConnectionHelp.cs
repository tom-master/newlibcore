using System;
using System.Collections.Concurrent;
using StackExchange.Redis;

namespace NewLibCore.Data.Redis.InternalHelper
{
	/// <summary>
	/// ConnectionMultiplexer对象管理帮助类
	/// </summary>
	internal static class RedisConnectionHelp
	{
		//系统自定义Key前缀
		public static readonly String SysCustomKey = "";

		private static readonly Object _locker = new Object();
		private static ConnectionMultiplexer _instance;
		private static readonly ConcurrentDictionary<String, ConnectionMultiplexer> _connectionCache = new ConcurrentDictionary<String, ConnectionMultiplexer>();

		/// <summary>
		/// 单例获取
		/// </summary>
		public static ConnectionMultiplexer Instance
		{
			get
			{
				if (_instance == null)
				{
					lock (_locker)
					{
						if (_instance == null || !_instance.IsConnected)
						{
							_instance = GetManager();
						}
					}
				}
				return _instance;
			}
		}

		/// <summary>
		/// 缓存获取
		/// </summary>
		/// <param name="connectionString"></param>
		/// <returns></returns>
		public static ConnectionMultiplexer GetConnectionMultiplexer(String connectionString)
		{
            if (String.IsNullOrEmpty(connectionString))
            {
                throw new ArgumentException("connectionString不能为空");
            }

			if (!_connectionCache.ContainsKey(connectionString))
			{
				_connectionCache[connectionString] = GetManager(connectionString);
			}
			return _connectionCache[connectionString];
		}

		private static ConnectionMultiplexer GetManager(String connectionString = null)
		{
			var connect = ConnectionMultiplexer.Connect(connectionString);
			connect.PreserveAsyncOrder = false;
			//注册如下事件
			connect.ConnectionFailed += MuxerConnectionFailed;
			connect.ConnectionRestored += MuxerConnectionRestored;
			connect.ErrorMessage += MuxerErrorMessage;
			connect.ConfigurationChanged += MuxerConfigurationChanged;
			connect.HashSlotMoved += MuxerHashSlotMoved;
			connect.InternalError += MuxerInternalError;

			return connect;
		}

		#region 事件

		/// <summary>
		/// 配置更改时
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private static void MuxerConfigurationChanged(Object sender, EndPointEventArgs e)
		{
			Console.WriteLine("Configuration changed: " + e.EndPoint);
		}

		/// <summary>
		/// 发生错误时
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private static void MuxerErrorMessage(Object sender, RedisErrorEventArgs e)
		{
			Console.WriteLine("ErrorMessage: " + e.Message);
		}

		/// <summary>
		/// 重新建立连接之前的错误
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private static void MuxerConnectionRestored(Object sender, ConnectionFailedEventArgs e)
		{
			Console.WriteLine("ConnectionRestored: " + e.EndPoint);
		}

		/// <summary>
		/// 连接失败 ， 如果重新连接成功你将不会收到这个通知
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private static void MuxerConnectionFailed(Object sender, ConnectionFailedEventArgs e)
		{
			Console.WriteLine("重新连接：Endpoint failed: " + e.EndPoint + ", " + e.FailureType + (e.Exception == null ? "" : (", " + e.Exception.Message)));
		}

		/// <summary>
		/// 更改集群
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private static void MuxerHashSlotMoved(Object sender, HashSlotMovedEventArgs e)
		{
			Console.WriteLine("HashSlotMoved:NewEndPoint" + e.NewEndPoint + ", OldEndPoint" + e.OldEndPoint);
		}

		/// <summary>
		/// redis类库错误
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private static void MuxerInternalError(Object sender, InternalErrorEventArgs e)
		{
			Console.WriteLine("InternalError:Message" + e.Exception.Message);
		}

		#endregion 事件
	}
}