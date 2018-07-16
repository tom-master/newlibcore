/* Copyright 2010-2014 MongoDB Inc.
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

using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using NewLib.Data.Mongodb.Core.Core.Bindings;
using NewLib.Data.Mongodb.Core.Core.Operations.ElementNameValidators;
using NewLib.Data.Mongodb.Core.Core.WireProtocol.Messages.Encoders;

namespace NewLib.Data.Mongodb.Core.Core.Operations
{
    internal class BulkUpdateOperationEmulator : BulkUnmixedWriteOperationEmulatorBase
    {
        // constructors
        public BulkUpdateOperationEmulator(
            CollectionNamespace collectionNamespace,
            IEnumerable<UpdateRequest> requests,
            MessageEncoderSettings messageEncoderSettings)
            : base(collectionNamespace, requests, messageEncoderSettings)
        {
        }

        // methods
        protected override WriteConcernResult ExecuteProtocol(IChannelHandle channel, WriteRequest request, CancellationToken cancellationToken)
        {
            var updateRequest = (UpdateRequest)request;

            return channel.Update(
                CollectionNamespace,
                MessageEncoderSettings,
                WriteConcern,
                updateRequest.Filter,
                updateRequest.Update,
                ElementNameValidatorFactory.ForUpdateType(updateRequest.UpdateType),
                updateRequest.IsMulti,
                updateRequest.IsUpsert,
                cancellationToken);
        }

        protected override Task<WriteConcernResult> ExecuteProtocolAsync(IChannelHandle channel, WriteRequest request, CancellationToken cancellationToken)
        {
            var updateRequest = (UpdateRequest)request;

            return channel.UpdateAsync(
                CollectionNamespace,
                MessageEncoderSettings,
                WriteConcern,
                updateRequest.Filter,
                updateRequest.Update,
                ElementNameValidatorFactory.ForUpdateType(updateRequest.UpdateType),
                updateRequest.IsMulti,
                updateRequest.IsUpsert,
                cancellationToken);
        }
    }
}
