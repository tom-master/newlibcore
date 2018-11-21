using System;
using System.Collections.Generic;
using System.Net.Sockets;

namespace Yikating.Service.HardwareService
{
    internal class BufferPoolManager : IDisposable
    {
        private byte[] _buffers;
        private int _bufferSize;
        private int _allSize;
        private int _currentIndex;
        private Stack<int> _freeIndexs;

        internal BufferPoolManager(int buffersSize, int defaultSize)
        {
            _bufferSize = defaultSize;
            _allSize = buffersSize;
            _currentIndex = 0;
            _buffers = new byte[_allSize];
            _freeIndexs = new Stack<int>();
            LoggerMessage.Write(String.Format("[info]---设置缓冲区：实际大小：{0} 默认大小：{1}", buffersSize, defaultSize));
        }
        /// <summary>
        /// 为每一个socket连接分配一块内存
        /// </summary>
        /// <param name="e"></param>
        /// <returns></returns>
        internal bool SetBuffer(SocketAsyncEventArgs e)
        {
            if (_freeIndexs.Count > 0)
            {
                e.SetBuffer(_buffers, _freeIndexs.Pop(), _bufferSize);
            }
            else
            {
                if ((_allSize - _currentIndex) < _bufferSize)
                {
                    return false;
                }
                e.SetBuffer(_buffers, _currentIndex, _bufferSize);
                _currentIndex += _bufferSize;
            }
            return true;
        }

        internal void FreeBuffer(SocketAsyncEventArgs e)
        {
            _freeIndexs.Push(e.Offset);
            for (int i = e.Offset; i < e.Offset + _bufferSize; i++)
            {
                if (_buffers[i] == 0)
                {
                    break;
                }

                _buffers[i] = 0;
            }
            e.SetBuffer(null, 0, 0);
        }

        public void Dispose()
        {
            _buffers = null;
            _freeIndexs = null;
        }
    }
}
