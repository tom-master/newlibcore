using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Text;
using System.Threading;

namespace ConsoleApp1.HardwareService
{
    public class SocketServer
    {
        private SocketPoolManager _pool;
        private static readonly Object _sync = new Object();
        private IDictionary<String, String> _deviceMapper = new Dictionary<String, String>();
        public void SocketInit()
        {
            var remoteIp = "192.168.0.100";
            var remotePort = 8090;
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
            LoggerMessage.Write(String.Format("[info]---开始监听{0}", remoteIp + ":" + remotePort));
            LoggerMessage.Write("[info]---socket池初始化完成。。。等待客户端连接");
        }

        private void ClearOfflineDeviceMapper()
        {
            try
            {
                var onlines = _pool.GetOnlines();
                var keys = _deviceMapper.Keys.ToArray();
                for (int i = 0; i < keys.Length; i++)
                {
                    var value = _deviceMapper[keys[i]];
                    if (!onlines.Any(d => d == value))
                    {
                        _deviceMapper.Remove(keys[i]);
                    }
                }
            }
            catch (Exception ex)
            {
                LoggerMessage.Write(String.Format("[err]---ClearOfflineDeviceMapper:出现异常：{0}", ex.Message));
            }


        }

        public void SendMessage(String command)
        {
            try
            {
                //command 格式 GUID--devicecode--(OPEN|CLOSE|QUERY)
                ClearOfflineDeviceMapper();
                if (_deviceMapper.Count <= 0)
                {
                    return;
                }
                foreach (var key in _deviceMapper.Keys)
                {
                    var commands = command.Split(new[] { "--" }, StringSplitOptions.RemoveEmptyEntries);
                    if ((key).Contains(commands[1]))
                    {
                        ClearOfflineDeviceMapper();
                        if (_deviceMapper.ContainsKey(key))
                        {
                            var ip = _deviceMapper[key];
                            _pool.SendMessage(ip, commands[0] + "--" + commands[2]);
                            LoggerMessage.Write(String.Format("[info]---向客户端：{0} 发送数据：{1}", ip, commands[0] + "--" + commands[2]));
                            break;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                LoggerMessage.Write(String.Format("[err]---SendMessage:出现异常：{0}", ex.Message));
            }
        }

        private void Pool_OnReceive(String uid, String data)
        {
            try
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
                            LoggerMessage.Write(String.Format("[info]---从客户端：{0} 接收数据：{1}", uid, data));
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                LoggerMessage.Write(String.Format("[err]---Pool_OnReceive:出现异常：{0}", ex.Message));
            }
        }
    }
}
