using System;
using System.Collections;
using System.Net.Sockets;

namespace ConsoleApp1.HardwareService
{
    /// <summary>
    /// 连接单元
    /// </summary>
    internal class ConnectionEntry : IDisposable
    { 
        internal Socket Client { get; set; }
         
        internal ArrayList TempArray { get; set; }

        public String Uid { get; set; }

        public ConnectionEntry(String UID)
        {
            Uid = UID;
            TempArray = new ArrayList();
        }

        public ConnectionEntry() : this("-1") { }

        public bool State { get; set; }

        public SocketAsyncEventArgs SendArg { get; set; }

        public SocketAsyncEventArgs RecArg { get; set; }

        public void Dispose()
        {
            if (SendArg != null)
            {
                SendArg.Dispose();
            }
            if (RecArg != null)
            {
                RecArg.Dispose();
            }
            SendArg = null;
            RecArg = null;
        }
    }
}
