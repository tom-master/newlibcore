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

using NewLibCore.Data.Mongodb.Bson.IO;
using NewLibCore.Data.Mongodb.Bson.ObjectModel;
using NewLibCore.Data.Mongodb.Core.Core.Connections;
using NewLibCore.Data.Mongodb.Core.Core.Misc;
using NewLibCore.Data.Mongodb.Core.Core.WireProtocol.Messages;
using NewLibCore.Data.Mongodb.Core.Core.WireProtocol.Messages.Encoders;

namespace NewLibCore.Data.Mongodb.Core.Core.WireProtocol
{
    internal class UpdateWireProtocol : WriteWireProtocolBase
    {
        // fields
        private readonly bool _isMulti;
        private readonly bool _isUpsert;
        private readonly BsonDocument _query;
        private readonly BsonDocument _update;
        private readonly IElementNameValidator _updateValidator;

        // constructors
        public UpdateWireProtocol(
            CollectionNamespace collectionNamespace,
            MessageEncoderSettings messageEncoderSettings,
            WriteConcern writeConcern,
            BsonDocument query,
            BsonDocument update,
            IElementNameValidator updateValidator,
            bool isMulti,
            bool isUpsert)
            : base(collectionNamespace, messageEncoderSettings, writeConcern)
        {
            _updateValidator = Ensure.IsNotNull(updateValidator, nameof(updateValidator));
            _query = Ensure.IsNotNull(query, nameof(query));
            _update = Ensure.IsNotNull(update, nameof(update));
            _isMulti = isMulti;
            _isUpsert = isUpsert;
        }

        // methods
        protected override RequestMessage CreateWriteMessage(IConnection connection)
        {
            return new UpdateMessage(
                RequestMessage.GetNextRequestId(),
                CollectionNamespace,
                _query,
                _update,
                _updateValidator,
                _isMulti,
                _isUpsert);
        }
    }
}
