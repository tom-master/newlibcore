using System;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;

namespace ConsoleApp1.HardwareService
{

    public class SocketPoolManager : IDisposable
    {
        private Mutex _mutex = new Mutex();

        private Semaphore _semaphoreAccept;

        private static ManualResetEvent _acceptLock = new ManualResetEvent(false);

        private static ManualResetEvent _sendLock = new ManualResetEvent(false);

        private int _maxConnect;

        private int _currentConnect;

        private BufferPoolManager _buffer;

        private SocketAsyncEventArgsPool _pool;

        private Socket _server;

        public delegate void AcceptHandler(String uid);

        public delegate void SendHandler(String uid, String result);

        public delegate void RecevieHandler(String uid, String data);

        public event AcceptHandler OnAccept;

        public event SendHandler OnSend;

        public event RecevieHandler OnReceive;

        public SocketPoolManager(int buffersize, int maxCount)
        {
            _buffer = new BufferPoolManager(buffersize * maxCount, buffersize);
            _currentConnect = 0;
            _maxConnect = maxCount;
            _currentConnect = 0;
            _pool = new SocketAsyncEventArgsPool();
            _semaphoreAccept = new Semaphore(maxCount - 1, maxCount - 1);
            LoggerMessage.Write(String.Format("[info]:设置socket池,缓冲区：{0},最大连接数:{1}", buffersize, maxCount));
            InitPool();
        }

        private void InitPool()
        {
            ConnectionEntry entry = null;
            for (var i = 0; i < _maxConnect; i++)
            {
                entry = new ConnectionEntry();
                entry.Uid = "-1";
                entry.RecArg = new SocketAsyncEventArgs();
                entry.RecArg.Completed += new EventHandler<SocketAsyncEventArgs>(RecArg_Completed);
                entry.SendArg = new SocketAsyncEventArgs();
                entry.SendArg.Completed += new EventHandler<SocketAsyncEventArgs>(SendArg_Completed);
                _pool.Push(entry);
            }
            LoggerMessage.Write(String.Format("[info]---初始化连接池完成"));
        }

        public void RunPool(String ipAddress, int port)
        {
            var endpoint = new IPEndPoint(IPAddress.Parse(ipAddress), port);
            _server = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            _server.Bind(endpoint);
            _server.Listen(100);
            LoggerMessage.Write("[info]---初始化socket完成");
            _server.ReceiveTimeout = 15 * 1000;
            uint dummy = 0;

            var inOptionValues = new byte[Marshal.SizeOf(dummy) * 3];
            BitConverter.GetBytes((uint)1).CopyTo(inOptionValues, 0);//是否启用Keep-Alive
            BitConverter.GetBytes((uint)5000).CopyTo(inOptionValues, Marshal.SizeOf(dummy));//多长时间开始第一次探测
            BitConverter.GetBytes((uint)15000).CopyTo(inOptionValues, Marshal.SizeOf(dummy) * 2);//探测时间间隔
            var outValue = BitConverter.GetBytes((uint)123456);

            _server.IOControl(IOControlCode.KeepAliveValues, inOptionValues, outValue);
            LoggerMessage.Write("[info]---Keep-Alive设置完成");

            Accept();
            _mutex.WaitOne();
        }
        private void Accept()
        {
            var callback = new Action(delegate ()
            {
                while (true)
                {
                    var e = new SocketAsyncEventArgs();
                    e.Completed += new EventHandler<SocketAsyncEventArgs>(Accept_Completed);

                    _acceptLock.Reset();
                    _server.AcceptAsync(e); 
                    _acceptLock.WaitOne();
                }
            });
            callback.BeginInvoke(null, null);
        }

        public void StopPool()
        {
            if (_server != null)
            {
                _server.Close();
            }
            _mutex.ReleaseMutex();

            Dispose();
        }

        public String[] GetOnlines()
        {
            return _pool.GetOnlines();
        }

        public void SendMessage(String message)
        {
            var onlines = _pool.GetOnlines();
            if (onlines.Length <= 0)
            {
                return;
            }
            SendMessage(onlines.FirstOrDefault(), message);
        }

        public void SendMessage(String uid, String message)
        {
            _sendLock.Reset();
            var entry = _pool.GetConnectionUnitByUID(uid);
      
            if (entry == null)
            {
                if (OnSend != null)
                {
                    OnSend(uid, "100");
                }
                _sendLock.Set();
                return;
            }
            var datas = Encoding.ASCII.GetBytes(message);
            entry.SendArg.SetBuffer(datas, 0, datas.Length);
            entry.Client.SendAsync(entry.SendArg);
            _sendLock.WaitOne();
        }

        void SendArg_Completed(object sender, SocketAsyncEventArgs e)
        {
            var client = sender as Socket;
            var entry = e.UserToken as ConnectionEntry;
          
            if (e.SocketError == SocketError.Success)
            {
                if (OnSend != null)
                {
                    OnSend(entry.Uid, "200");
                }
                else if (OnSend != null)
                {
                    OnSend(entry.Uid, "101");
                }
            }
            _sendLock.Set();
        }

        void RecArg_Completed(object sender, SocketAsyncEventArgs e)
        {
            var client = sender as Socket;
            var entry = e.UserToken as ConnectionEntry;
            if (e.SocketError == SocketError.Success)
            {
                var rec = e.BytesTransferred;
                if (rec == 0)
                {
                    CloseSocket(entry);
                    return;
                }

                if (client.Available > 0)
                {
                    entry.TempArray.AddRange(e.Buffer);
                    _buffer.FreeBuffer(entry.RecArg);
                    _buffer.SetBuffer(entry.RecArg);
                    client.SendAsync(entry.RecArg);
                    return;
                }
                var data = e.Buffer.Where(w => w != 0).ToArray();
                var len = rec;

                if (entry.TempArray.Count != 0)
                {
                    foreach (var item in data)
                    {
                        if (item == 0)
                        {
                            break;
                        }
                        entry.TempArray.Add(item);
                    }
                    data = entry.TempArray.ToArray(typeof(byte)) as byte[];
                    rec = data.Length;
                    entry.TempArray.Clear();
                }

                var dataStr = Encoding.ASCII.GetString(data, 0, len);
                if (OnReceive != null)
                {
                    OnReceive(entry.Uid, dataStr);
                }
                if (!entry.State)
                {
                    return;
                }

                _buffer.FreeBuffer(e);
                _buffer.SetBuffer(e);
                client.ReceiveAsync(e);
            }
            else
            {
                CloseSocket(entry);
            }
        }

        void Accept_Completed(object sender, SocketAsyncEventArgs e)
        {
            var client = e.AcceptSocket;
            try
            {
                if (client.Connected)
                {
                    var point = client.RemoteEndPoint as IPEndPoint;
                    var uid = point.Address + ":" + point.Port;
                    var entry = _pool.Pop(uid);
                    entry.Client = client;
                    entry.State = true;
                    entry.Uid = uid;
                    entry.RecArg.UserToken = entry;
                    entry.SendArg.UserToken = entry;
                    _buffer.SetBuffer(entry.RecArg);
                     
                    client.ReceiveAsync(entry.RecArg); 
                    _semaphoreAccept.WaitOne();
                    Interlocked.Increment(ref _currentConnect);

                    if (OnAccept != null)
                    {
                        OnAccept(uid);
                    }
                    LoggerMessage.Write(String.Format("[info]---客户端:{0} 已连接", uid));
                }
                else if (client != null)
                {
                    client.Close();
                    client.Dispose();
                    client = null;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.ToString());
            } 
            _acceptLock.Set();
            e.Dispose();
        }
         
        private void CloseSocket(ConnectionEntry entry)
        {
            if (entry.Client != null)
            {
                entry.Client.Shutdown(SocketShutdown.Both);
                entry.Client.Dispose();
                entry.Client = null;
            }
            LoggerMessage.Write(String.Format("[info]---客户端:{0} 已断开连接", entry.Uid));
            _pool.Push(entry);
            _semaphoreAccept.Release();

            Interlocked.Decrement(ref _currentConnect);
        }

        public void Dispose()
        {
            if (_pool != null)
            {
                _pool.Dispose();
                _pool = null;
            }
            if (_buffer != null)
            {
                _buffer.Dispose();
                _buffer = null;
            }
            if (_server != null)
            {
                _server.Dispose();
                _server = null;
            }
        }
    }
}
