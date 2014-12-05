-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.DynamoDb.Streams.Commands.GetShardIterator
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache-2.0
--
module Aws.DynamoDb.Streams.Commands.GetShardIterator
( -- * Request
  GetShardIterator(..)
, getShardIterator
  -- ** Lenses
, gsiSequenceNumber
, gsiShardId
, gsiShardIteratorType
, gsiStreamId

  -- * Response
, GetShardIteratorResponse(..)
  -- ** Lenses
, gsirShardIterator
) where

import Aws.Core
import Aws.DynamoDb.Streams.Core
import Aws.DynamoDb.Streams.Types
import Control.Applicative
import Control.Applicative.Unicode
import Data.Aeson
import Data.Typeable

-- | A shard iterator provides information about how to retrieve stream records
-- from within a shard. Note: A shard iterator expires 15 minutes after it is
-- returned to the requester.
--
data GetShardIterator
  = GetShardIterator
  { _gsiSequenceNumber ∷ !(Maybe SequenceNumber)
    -- ^ The sequence number of a stream record in the shard from which to
    -- start reading.

  , _gsiShardId ∷ !ShardId
    -- ^ The identifier of the shard.

  , _gsiShardIteratorType ∷ !ShardIteratorType
    -- ^ Determines how the shard iterator is used to start reading stream
    -- records from the shard.

  , _gsiStreamId ∷ !StreamId
  } deriving (Eq, Ord, Read, Show, Typeable)

-- | A basic 'GetShardIterator' request for a given stream id.
--
-- @
-- myRequest =
--   'getShardIterator' myStream myShard 'ShardIteratorLatest'
--     & 'gsiSequenceNumber' ?~ sqn
-- @
--
getShardIterator
  ∷ StreamId
  → ShardId
  → ShardIteratorType
  → GetShardIterator
getShardIterator stream shard iteratorType =
  GetShardIterator
    { _gsiStreamId = stream
    , _gsiShardIteratorType = iteratorType
    , _gsiShardId = shard
    , _gsiSequenceNumber = Nothing
    }

instance ToJSON GetShardIterator where
  toJSON GetShardIterator{..} = object
    [ "SequenceNumber" .= _gsiSequenceNumber
    , "ShardId" .= _gsiShardId
    , "ShardIteratorType" .= _gsiShardIteratorType
    , "StreamId" .= _gsiStreamId
    ]

instance FromJSON GetShardIterator where
  parseJSON =
    withObject "GetShardIterator" $ \o →
      pure GetShardIterator
        ⊛ o .:? "SequenceNumber"
        ⊛ o .: "ShardId"
        ⊛ o .: "ShardIteratorType"
        ⊛ o .: "StreamId"

-- | A lens for '_gsiSequenceNumber'.
--
-- @
-- 'gsiSequenceNumber' ∷ Lens' 'GetShardIterator' ('Maybe' 'SequenceNumber')
-- @
--
gsiSequenceNumber
  ∷ Functor f
  ⇒ (Maybe SequenceNumber → f (Maybe SequenceNumber))
  → GetShardIterator
  → f GetShardIterator
gsiSequenceNumber i GetShardIterator{..} =
  (\_gsiSequenceNumber → GetShardIterator{..})
    <$> i _gsiSequenceNumber

-- | A lens for '_gsiShardId'.
--
-- @
-- 'gsiShardId' ∷ Lens' 'GetShardIterator' 'ShardId'
-- @
--
gsiShardId
  ∷ Functor f
  ⇒ (ShardId → f ShardId)
  → GetShardIterator
  → f GetShardIterator
gsiShardId i GetShardIterator{..} =
  (\_gsiShardId → GetShardIterator{..})
    <$> i _gsiShardId

-- | A lens for '_gsiShardIteratorType'.
--
-- @
-- 'gsiShardIteratorType' ∷ Lens' 'GetShardIterator' 'ShardIteratorType'
-- @
--
gsiShardIteratorType
  ∷ Functor f
  ⇒ (ShardIteratorType → f ShardIteratorType)
  → GetShardIterator
  → f GetShardIterator
gsiShardIteratorType i GetShardIterator{..} =
  (\_gsiShardIteratorType → GetShardIterator{..})
    <$> i _gsiShardIteratorType

-- | A lens for '_gsiStreamId'.
--
-- @
-- 'gsiStreamId' ∷ Lens' 'GetShardIterator' 'StreamId'
-- @
--
gsiStreamId
  ∷ Functor f
  ⇒ (StreamId → f StreamId)
  → GetShardIterator
  → f GetShardIterator
gsiStreamId i GetShardIterator{..} =
  (\_gsiStreamId → GetShardIterator{..})
    <$> i _gsiStreamId


data GetShardIteratorResponse
  = GetShardIteratorResponse
  { _gsirShardIterator ∷ !ShardIterator
  } deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON GetShardIteratorResponse where
  toJSON GetShardIteratorResponse{..} = object
    [ "ShardIterator" .= _gsirShardIterator
    ]

instance FromJSON GetShardIteratorResponse where
  parseJSON =
    withObject "GetShardIteratorResponse" $ \o →
      GetShardIteratorResponse
        <$> o .: "ShardIterator"

-- | A lens for '_gsirShardIterator'.
--
-- @
-- 'gsirShardIterator' ∷ Lens' 'GetShardIteratorResponse' 'ShardIterator'
-- @
--
gsirShardIterator
  ∷ Functor f
  ⇒ (ShardIterator → f ShardIterator)
  → GetShardIteratorResponse
  → f GetShardIteratorResponse
gsirShardIterator i GetShardIteratorResponse{..} =
  (\_gsirShardIterator → GetShardIteratorResponse{..})
    <$> i _gsirShardIterator

instance ResponseConsumer r GetShardIteratorResponse where
  type ResponseMetadata GetShardIteratorResponse = StreamsMetadata
  responseConsumer _ = streamsResponseConsumer

instance SignQuery GetShardIterator where
  type ServiceConfiguration GetShardIterator = StreamsConfiguration
  signQuery cmd = streamsSignQuery StreamsQuery
    { _stqAction = ActionGetShardIterator
    , _stqBody = encode cmd
    }

instance Transaction GetShardIterator GetShardIteratorResponse

instance AsMemoryResponse GetShardIteratorResponse where
  type MemoryResponse GetShardIteratorResponse = GetShardIteratorResponse
  loadToMemory = return

