﻿/* Copyright 2010-2014 MongoDB Inc.
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

namespace NewLib.Data.Mongodb.Bson.Serialization.Attributes
{
    /// <summary>
    /// Represents an attribute used to modify a creator map.
    /// </summary>
    public interface IBsonCreatorMapAttribute
    {
        /// <summary>
        /// Applies the attribute to the creator map.
        /// </summary>
        /// <param name="creatorMap">The creator map.</param>
        void Apply(BsonCreatorMap creatorMap);
    }
}
