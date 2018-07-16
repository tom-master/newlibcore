/* Copyright 2013-2015 MongoDB Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

using System;
using System.Threading;
using System.Threading.Tasks;
using NewLib.Data.Mongodb.Core.Core.Misc;
using NewLib.Data.Mongodb.Core.Core.Servers;

namespace NewLib.Data.Mongodb.Core.Core.Bindings
{
    /// <summary>
    /// Represents a read-write binding that is bound to a channel.
    /// </summary>
    public sealed class ChannelReadWriteBinding : IReadWriteBinding
    {
        // fields
        private readonly IChannelHandle _channel;
        private bool _disposed;
        private readonly IServer _server;

        // constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="ChannelReadWriteBinding"/> class.
        /// </summary>
        /// <param name="server">The server.</param>
        /// <param name="channel">The channel.</param>
        public ChannelReadWriteBinding(IServer server, IChannelHandle channel)
        {
            _server = Ensure.IsNotNull(server, nameof(server));
            _channel = Ensure.IsNotNull(channel, nameof(channel));
        }

        // properties
        /// <inheritdoc/>
        public ReadPreference ReadPreference
        {
            get { return ReadPreference.Primary; }
        }

        // methods
        /// <inheritdoc/>
        public void Dispose()
        {
            if (!_disposed)
            {
                _channel.Dispose();
                _disposed = true;
                GC.SuppressFinalize(this);
            }
        }

        /// <inheritdoc/>
        public IChannelSourceHandle GetReadChannelSource(CancellationToken cancellationToken)
        {
            ThrowIfDisposed();
            return GetChannelSourceHelper();
        }

        /// <inheritdoc/>
        public Task<IChannelSourceHandle> GetReadChannelSourceAsync(CancellationToken cancellationToken)
        {
            ThrowIfDisposed();
            return Task.FromResult(GetChannelSourceHelper());
        }

        /// <inheritdoc/>
        public IChannelSourceHandle GetWriteChannelSource(CancellationToken cancellationToken)
        {
            ThrowIfDisposed();
            return GetChannelSourceHelper();
        }

        /// <inheritdoc/>
        public Task<IChannelSourceHandle> GetWriteChannelSourceAsync(CancellationToken cancellationToken)
        {
            ThrowIfDisposed();
            return Task.FromResult(GetChannelSourceHelper());
        }

        // private methods
        private IChannelSourceHandle GetChannelSourceHelper()
        {
            return new ChannelSourceHandle(new ChannelChannelSource(_server, _channel.Fork()));
        }

        private void ThrowIfDisposed()
        {
            if (_disposed)
            {
                throw new ObjectDisposedException(GetType().Name);
            }
        }
    }
}
