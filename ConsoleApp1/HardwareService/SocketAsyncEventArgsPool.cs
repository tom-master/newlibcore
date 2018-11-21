using System;
using System.Collections.Generic;
using System.Linq;

namespace ConsoleApp1.HardwareService
{

    internal class SocketAsyncEventArgsPool : IDisposable
    {
        private Dictionary<String, ConnectionEntry> _busyCollection;
        private Stack<ConnectionEntry> _freeCollecton;

        internal SocketAsyncEventArgsPool()
        {
            _busyCollection = new Dictionary<String, ConnectionEntry>();
            _freeCollecton = new Stack<ConnectionEntry>();
        }

        internal ConnectionEntry Pop(String uid)
        {
            var unit = _freeCollecton.Pop();
            unit.State = true;
            unit.Uid = uid;
            _busyCollection.Add(uid, unit);

            LoggerMessage.Write(String.Format("[info]---已使用连接{0}  未使用连接{1}", _busyCollection.Count, _freeCollecton.Count));
            return unit;
        }

        internal void Push(ConnectionEntry unit)
        {
            if (!String.IsNullOrEmpty(unit.Uid) && unit.Uid != "-1")
            {
                _busyCollection.Remove(unit.Uid);
            }

            unit.Uid = "-1";
            unit.State = false;
            _freeCollecton.Push(unit);

            LoggerMessage.Write(String.Format("[info]---已使用连接{0}  未使用连接{1}", _busyCollection.Count, _freeCollecton.Count));
        }

        internal ConnectionEntry GetConnectionUnitByUID(String uid)
        {
            if (_busyCollection.ContainsKey(uid))
            {
                return _busyCollection[uid];
            }
            return null;
        }

        /// <summary>
        /// 获取在线的客户端列表
        /// </summary>
        internal String[] GetOnlines()
        {
            return _busyCollection.Keys.ToArray();
        }

        public void Dispose()
        {
            foreach (var item in _busyCollection)
            {
                item.Value.Dispose();
            }

            _busyCollection.Clear();

            while (_freeCollecton.Count > 0)
            {
                _freeCollecton.Pop().Dispose();
            }
        }
    }
}
