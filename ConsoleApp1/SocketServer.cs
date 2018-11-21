using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Text;
using Yikating.Service.HardwareService;

namespace Yikating.Service
{
    public class SocketServer
    {
        private SocketPoolManager _pool;
        private SubscriberService _subscriber;
        private static readonly Object _sync = new Object();
        private IDictionary<String, String> _deviceMapper = new Dictionary<String, String>();

        public void SocketInit(SubscriberService subscriber)
        {
            var remoteIp = ConfigurationManager.AppSettings["remoteIp"];
            var remotePort = Int32.Parse(ConfigurationManager.AppSettings["remotePort"]);
            var maxConnections = ConfigurationManager.AppSettings["maxConnections"];
            if (!String.IsNullOrEmpty(maxConnections))
            {
                _pool = new SocketPoolManager(32 * 1024, Int32.Parse(maxConnections));
            }
            else
            {
                _pool = new SocketPoolManager(32 * 1024, 10);
            }
            _pool.OnReceive += new SocketPoolManager.RecevieHandler(Pool_OnReceive);
            _pool.RunPool(remoteIp, remotePort);
            _subscriber = subscriber;
            LoggerMessage.Write(String.Format("[info]---开始监听{0}", remoteIp + ":" + remotePort));
            LoggerMessage.Write("[info]---socket池初始化完成。。。等待客户端连接");
        }

        private void ClearOfflineDeviceMapper()
        {
            while (true)
            {
                var onlines = _pool.GetOnlines();
                var temp = _deviceMapper.ToDictionary(k => k.Value, v => v.Key);
                var mapperExcept = _deviceMapper.Keys.Except(onlines);
                
            }
        }

        public void SendMessage(String command)
        {
            //command 格式 GUID--devicecode--(OPEN|CLOSE|QUERY)
            if (_deviceMapper.Count <= 0)
            {
                return;
            }

            foreach (var key in _deviceMapper.Keys)
            {
                var commands = command.Split(new[] { "--" }, StringSplitOptions.RemoveEmptyEntries);
                if ((key).Contains(commands[1]))
                {
                    var ip = _deviceMapper[key];
                    if (_pool.GetOnlines().Any(a => a == ip))
                    {
                        _pool.SendMessage(ip, commands[0] + "--" + commands[2]);
                        LoggerMessage.Write(String.Format("[info]---向客户端：{0} 发送数据：{1}", ip, commands[0] + "--" + commands[2]));
                    }
                }
            }
        }

        private void Pool_OnReceive(String uid, String data)
        {
            if (data.Contains("123456"))
            {
                return;
            }

            if (data.IndexOf("--") > 0)
            {
                var datas = data.Split(new[] { "--" }, StringSplitOptions.RemoveEmptyEntries);
                Boolean isSuccess;
                if (Boolean.TryParse(datas[1], out isSuccess))
                {
                    var dataByte = Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(new { OpenId = datas[0], State = isSuccess ? 1 : 0 }));
                    var newBytes = new byte[1];
                    newBytes[0] = 127;
                    _subscriber.PublishAsync(newBytes.Concat(dataByte).ToArray());

                    LoggerMessage.Write(String.Format("[info]---{0}{1}", uid, isSuccess));
                }
            }
            else
            {
                lock (_sync)
                {
                    var key = /*uid + ":" +*/ data;

                    if (!_deviceMapper.ContainsKey(key))
                    {
                        _deviceMapper.Add(key, uid);
                        //_deviceMapper.Remove(key);
                    }
                }
            }
            LoggerMessage.Write(String.Format("[info]---从客户端：{0} 接收数据：{1}", uid, data));
        }
    }
}
