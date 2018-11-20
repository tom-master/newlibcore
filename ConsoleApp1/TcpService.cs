using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace ConsoleApp1
{
    public class TcpService
    {
        private readonly IDictionary<String, String> _mappers = new Dictionary<String, String>();
        private SocketPoolController pool;

        public TcpService()
        {
            Init();
        }

        private void Init()
        {
            pool = new SocketPoolController(32 * 1024, 20);
            pool.OnReceive += new SocketPoolController.RecevieHandler(Pool_OnReceive);
            pool.OnSend += new SocketPoolController.SendHandler(Pool_OnSend);
            pool.OnAccept += new SocketPoolController.AcceptHandler(Pool_OnAccept);
            pool.RunPool("192.168.0.100", 8090);
            LoggerMessage.Write($@"[tcp-info]--start {"192.168.0.100"}:{8090}");
        }


        public void SendMessage(String message)
        {
            if (!_mappers.Any())
            {
                return;
            }
            foreach (var key in _mappers.Keys)
            {
                if ((key.Split(new[] { ':' }, StringSplitOptions.RemoveEmptyEntries)[2]) == message)
                {
                    if (_mappers.ContainsKey(key))
                    {
                        var ip = _mappers[key];
                        pool.SendMessage(ip, message);
                    }
                }
            }
        }

        private void Pool_OnAccept(String uid) { }
        private void Pool_OnSend(String uid, String result) { }
        private void Pool_OnReceive(String uid, String data)
        {
            var key = $@"{uid}:{data}";
            if (_mappers.ContainsKey(key))
            {
                _mappers.Remove(key);
                LoggerMessage.Write($@"[tcp-info]--remove key:{key},value{uid}");
            }
            _mappers.Add(key, uid);

            LoggerMessage.Write($@"[tcp-info]--add key:{key},value{uid}");
        }

        class ConnectionUnit : IDisposable
        {
            private string _uid;//单元的编号，默认为-1
            private bool _state;//单元的状态，true表示使用，false表示空闲
            private SocketAsyncEventArgs _sendArg;//专用于发送
            private SocketAsyncEventArgs _recArg;//专用于接收
            internal Socket client { get; set; }//客户端的socket实例
            internal ArrayList tempArray { get; set; }//暂存已接收的数据，避免粘包用的

            public string Uid
            {
                get { return _uid; }
                set { _uid = value; }
            }

            public ConnectionUnit(string UID)
            {
                _uid = UID;
                tempArray = new ArrayList();
            }

            public ConnectionUnit() : this("-1") { }

            public bool State
            {
                get { return _state; }
                set { _state = value; }
            }

            public SocketAsyncEventArgs SendArg
            {
                get { return _sendArg; }
                set { _sendArg = value; }
            }

            public SocketAsyncEventArgs RecArg
            {
                get { return _recArg; }
                set { _recArg = value; }
            }

            public void Dispose()
            {
                if (_sendArg != null)
                    _sendArg.Dispose();
                if (_recArg != null)
                    _recArg.Dispose();

                _sendArg = null;
                _recArg = null;
            }
        }

        class BufferManager : IDisposable
        {
            private byte[] buffers;
            private int bufferSize;
            private int allSize;
            private int currentIndex;
            private Stack<int> freeIndexs;

            /// <summary>
            /// 构造缓存池
            /// </summary>
            /// <param name="buffersSize">池总大小</param>
            /// <param name="defaultSize">默认单元大小</param>
            internal BufferManager(int buffersSize, int defaultSize)
            {
                this.bufferSize = defaultSize;
                this.allSize = buffersSize;
                currentIndex = 0;
                this.buffers = new byte[allSize];
                freeIndexs = new Stack<int>();
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="e"></param>
            /// <param name="offSet"></param>
            /// <returns></returns>
            internal bool SetBuffer(SocketAsyncEventArgs e)
            {
                if (freeIndexs.Count > 0)
                {
                    e.SetBuffer(buffers, freeIndexs.Pop(), bufferSize);
                }
                else
                {
                    if ((allSize - currentIndex) < bufferSize) return false;
                    e.SetBuffer(buffers, 0, bufferSize);
                    currentIndex += bufferSize;
                }
                return true;
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="e"></param>
            internal void FreeBuffer(SocketAsyncEventArgs e)
            {
                freeIndexs.Push(e.Offset);
                for (int i = e.Offset; i < e.Offset + bufferSize; i++)
                {
                    if (buffers[i] == 0) break;
                    buffers[i] = 0;
                }
                e.SetBuffer(null, 0, 0);
            }

            public void Dispose()
            {
                buffers = null;
                freeIndexs = null;
            }
        }

        class SocketAsyncEventArgsPool : IDisposable
        {
            private Dictionary<string, ConnectionUnit> busyCollection;
            private Stack<ConnectionUnit> freeCollecton;

            internal SocketAsyncEventArgsPool()
            {
                busyCollection = new Dictionary<string, ConnectionUnit>();
                freeCollecton = new Stack<ConnectionUnit>();
            }

            /// <summary>
            /// 取出
            /// </summary>
            internal ConnectionUnit Pop(string uid)
            {
                ConnectionUnit unit = freeCollecton.Pop();
                unit.State = true;
                unit.Uid = uid;
                busyCollection.Add(uid, unit);
                LoggerMessage.Write(String.Format("[tcp-info]--busy：{0} free：{1}", busyCollection.Count, freeCollecton.Count));
                return unit;
            }

            /// <summary>
            /// 放回
            /// </summary>
            internal void Push(ConnectionUnit unit)
            {
                if (!string.IsNullOrEmpty(unit.Uid) && unit.Uid != "-1")
                    busyCollection.Remove(unit.Uid);
                unit.Uid = "-1";
                unit.State = false;
                freeCollecton.Push(unit);
                LoggerMessage.Write(String.Format("[tcp-info]--busy：{0} free：{1}", busyCollection.Count, freeCollecton.Count));
            }

            /// <summary>
            /// 获取
            /// </summary>
            internal ConnectionUnit GetConnectionUnitByUID(string uid)
            {
                if (busyCollection.ContainsKey(uid))
                    return busyCollection[uid];
                return null;
            }

            /// <summary>
            /// 
            /// </summary>
            internal string[] GetOnLineList()
            {
                return busyCollection.Keys.ToArray();
            }

            public void Dispose()
            {
                foreach (KeyValuePair<string, ConnectionUnit> item in busyCollection)
                    item.Value.Dispose();

                busyCollection.Clear();

                while (freeCollecton.Count > 0)
                    freeCollecton.Pop().Dispose();
            }
        }

        public class SocketPoolController : IDisposable
        {
            /// <summary>
            /// 初始化池的互斥体
            /// </summary>
            private Mutex mutex = new Mutex();

            /// <summary>
            /// Accept限制信号
            /// </summary>
            private Semaphore semaphoreAccept;

            /// <summary>
            /// Accept信号
            /// </summary>
            private static ManualResetEvent acceptLock = new ManualResetEvent(false);

            /// <summary>
            /// Send信号
            /// </summary>
            private static ManualResetEvent sendLock = new ManualResetEvent(false);

            /// <summary>
            /// 最大并发数(连接数)
            /// </summary>
            private int maxConnect;

            /// <summary>
            /// 当前连接数(并发数)
            /// </summary>
            private int currentConnect;

            /// <summary>
            /// 缓冲池
            /// </summary>
            private BufferManager buffer;

            /// <summary>
            /// SocketasyncEventArgs池
            /// </summary>
            private SocketAsyncEventArgsPool pool;

            /// <summary>
            /// 服务端Socket
            /// </summary>
            private Socket server;

            /// <summary>
            /// 完成接受的委托
            /// </summary>
            public delegate void AcceptHandler(string uid);

            /// <summary>
            /// 完成发送的委托
            /// </summary>
            public delegate void SendHandler(string uid, string result);

            /// <summary>
            /// 完成接收的委托
            /// </summary>
            public delegate void RecevieHandler(string uid, string data);

            /// <summary>
            /// 完成接受事件
            /// </summary>
            public event AcceptHandler OnAccept;

            /// <summary>
            /// 完成发送事件
            /// </summary>
            public event SendHandler OnSend;

            /// <summary>
            /// 完成接收事件
            /// </summary>
            public event RecevieHandler OnReceive;

            /// <summary>
            /// 构造函数
            /// </summary>
            /// <param name="buffersize">单元缓冲区大小</param>
            /// <param name="maxCount">并发总数</param>
            public SocketPoolController(int buffersize, int maxCount)
            {
                buffer = new BufferManager(buffersize * maxCount, buffersize);
                this.currentConnect = 0;
                this.maxConnect = maxCount;
                this.currentConnect = 0;
                this.pool = new SocketAsyncEventArgsPool();
                //设置并发数信号，经试验过是并发数-1才对
                this.semaphoreAccept = new Semaphore(maxCount - 1, maxCount - 1);
                InitPool();
            }

            /// <summary>
            /// 初始化SocketAsyncEventArgs池
            /// 这里主要是给空闲栈填充足够的实例
            /// </summary>
            private void InitPool()
            {
                ConnectionUnit unit = null;
                for (int i = 0; i < maxConnect; i++)
                {
                    unit = new ConnectionUnit();
                    unit.Uid = "-1";
                    unit.RecArg = new SocketAsyncEventArgs();
                    unit.RecArg.Completed += new EventHandler<SocketAsyncEventArgs>(RecArg_Completed);
                    unit.SendArg = new SocketAsyncEventArgs();
                    unit.SendArg.Completed += new EventHandler<SocketAsyncEventArgs>(SendArg_Completed);
                    this.pool.Push(unit);
                }
            }

            /// <summary>
            /// 启动池
            /// </summary>
            /// <param name="ipAddress">服务端的IP</param>
            /// <param name="port">端口</param>
            public void RunPool(string ipAddress, int port)
            {
                IPEndPoint endpoint = new IPEndPoint(IPAddress.Parse(ipAddress), port);
                server = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
                server.Bind(endpoint);
                server.Listen(100);

                //调用方法异步Accept客户端的连接
                MyAsyncAccept();
                //设置信号，防止再池在已经启动的情况下再次启动
                mutex.WaitOne();
            }

            /// <summary>
            /// 异步Accept客户端的连接
            /// </summary>
            void MyAsyncAccept()
            {
                //这里使用Action的方式异步循环接受客户端的连接
                //模仿同事的做法没开线程，不知这种方式是好是坏
                Action callback = new Action(delegate ()
                {
                    while (true)
                    {
                        //每次接受都要新开一个SocketAsyncEventArgs,否则会报错
                        //其实我也想重复利用的
                        SocketAsyncEventArgs e = new SocketAsyncEventArgs();
                        e.Completed += new EventHandler<SocketAsyncEventArgs>(Accept_Completed);

                        acceptLock.Reset();
                        server.AcceptAsync(e);
                        //在异步接受完成之前阻塞当前线程
                        acceptLock.WaitOne();
                    }
                });
                callback.BeginInvoke(null, null);
            }


            /// <summary>
            /// 停止池
            /// </summary>
            public void StopPool()
            {
                //把服务端的socket关了
                if (server != null)
                    server.Close();
                //释放互斥信号，等待下次启动
                mutex.ReleaseMutex();
                //释放资源
                Dispose();
            }


            public String[] GetOnlines()
            {
                return pool.GetOnLineList();
            }

            /// <summary>
            /// 发送消息
            /// </summary>
            /// <param name="uid"></param>
            /// <param name="message"></param>
            public void SendMessage(string uid, string message)
            {
                sendLock.Reset();
                ConnectionUnit unit = pool.GetConnectionUnitByUID(uid);
                //如果获取不了连接单元就不发送了，
                if (unit == null)
                {
                    if (OnSend != null) OnSend(uid, "100");
                    sendLock.Set();
                    return;
                }
                byte[] datas = Encoding.ASCII.GetBytes(message);
                unit.SendArg.SetBuffer(datas, 0, datas.Length);
                unit.client.SendAsync(unit.SendArg);
                //阻塞当前线程，等到发送完成才释放
                sendLock.WaitOne();
            }

            void SendArg_Completed(object sender, SocketAsyncEventArgs e)
            {
                Socket client = sender as Socket;
                ConnectionUnit unit = e.UserToken as ConnectionUnit;
                //这里的消息码有三个，2字头的是成功的，1字头是不成功的
                //101是未知错误，100是客户端不在线
                if (e.SocketError == SocketError.Success)
                    if (OnSend != null) OnSend(unit.Uid, "200");
                    else if (OnSend != null) OnSend(unit.Uid, "101");
                //释放信号，以便下次发送消息执行
                sendLock.Set();
            }

            void RecArg_Completed(object sender, SocketAsyncEventArgs e)
            {
                Socket client = sender as Socket;
                ConnectionUnit unit = e.UserToken as ConnectionUnit;
                //这里大致与上一篇异步通信的一样，只是对缓冲区的处理有一点差异
                if (e.SocketError == SocketError.Success)
                {
                    int rec = e.BytesTransferred;
                    if (rec == 0)
                    {
                        CloseSocket(unit);
                        return;
                    }
                    if (client.Available > 0)
                    {
                        unit.tempArray.AddRange(e.Buffer);
                        buffer.FreeBuffer(unit.RecArg);
                        buffer.SetBuffer(unit.RecArg);
                        client.SendAsync(unit.RecArg);
                        return;
                    }
                    byte[] data = e.Buffer;
                    int len = rec;
                    if (unit.tempArray.Count != 0)
                    {
                        foreach (byte item in data)
                        {
                            if (item == 0) break;
                            unit.tempArray.Add(item);
                        }
                        data = unit.tempArray.ToArray(typeof(byte)) as byte[];
                        rec = data.Length;
                        unit.tempArray.Clear();
                    }

                    string dataStr = Encoding.ASCII.GetString(data, 0, len);
                    if (OnReceive != null)
                        OnReceive(unit.Uid, dataStr);

                    if (!unit.State) return;
                    buffer.FreeBuffer(e);
                    buffer.SetBuffer(e);
                    client.ReceiveAsync(e);
                }
                //这里还多个了一个关闭当前连接
                else
                {
                    CloseSocket(unit);
                }
            }

            void Accept_Completed(object sender, SocketAsyncEventArgs e)
            {
                Socket client = e.AcceptSocket;
                try
                {
                    if (client.Connected)
                    {
                        IPEndPoint point = client.RemoteEndPoint as IPEndPoint;
                        string uid = point.Address + ":" + point.Port;
                        ConnectionUnit unit = pool.Pop(uid);
                        unit.client = client;
                        unit.State = true;
                        unit.Uid = uid;
                        unit.RecArg.UserToken = unit;
                        unit.SendArg.UserToken = unit;
                        buffer.SetBuffer(unit.RecArg);

                        //在接受成功之后就开始接收数据了
                        client.ReceiveAsync(unit.RecArg);
                        //设置并发限制信号和增加当前连接数
                        semaphoreAccept.WaitOne();
                        Interlocked.Increment(ref currentConnect);

                        if (OnAccept != null) OnAccept(uid);
                        LoggerMessage.Write(String.Format("[tcp--info]:client connect {0}", unit.Uid));
                    }
                    else if (client != null)
                    {
                        client.Close();
                        client.Dispose();
                        client = null;
                    }
                }
                catch (Exception ex) { Console.WriteLine(ex.ToString()); }
                //设置Accept信号，以便下次Accept的执行
                acceptLock.Set();
                e.Dispose();
            }

            /// <summary>
            /// 关闭一个连接单元
            /// </summary>
            private void CloseSocket(ConnectionUnit unit)
            {
                //关闭并释放客户端socket的字眼
                if (unit.client != null)
                {
                    unit.client.Shutdown(SocketShutdown.Both);
                    unit.client.Dispose();
                    unit.client = null;
                }
                LoggerMessage.Write(String.Format("[tcp--info]:client disconnect {0}", unit.Uid));
                //Console.WriteLine(unit.Uid+" disconnect ");
                //把连接放回连接池
                pool.Push(unit);
                //释放并发信号
                semaphoreAccept.Release();
                //减少当前连接数
                Interlocked.Decrement(ref currentConnect);
            }

            public void Dispose()
            {
                if (pool != null)
                {
                    pool.Dispose();
                    pool = null;
                }
                if (buffer != null)
                {
                    buffer.Dispose();
                    buffer = null;
                }
                if (server != null)
                {
                    server.Dispose();
                    server = null;
                }

            }
        }

        public class LoggerMessage
        {
            public static void Write(String message)
            {
                Console.WriteLine(message);
            }
        }
    }
}
