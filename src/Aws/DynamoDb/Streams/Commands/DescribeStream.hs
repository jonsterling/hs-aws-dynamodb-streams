-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.DynamoDb.Streams.Commands.DescribeStream
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
module Aws.DynamoDb.Streams.Commands.DescribeStream
( -- * Request
  DescribeStream(..)
, describeStream
  -- ** Lenses
, dstExclusiveStartShardId
, dstLimit
, dstStreamArn

  -- * Response
, DescribeStreamResponse(..)
  -- ** Lenses
, dstrStreamDescription
) where

import Aws.Core
import Aws.General (Arn)
import Aws.DynamoDb.Streams.Core
import Aws.DynamoDb.Streams.Types
import Control.Applicative
import Control.Applicative.Unicode
import Data.Aeson
import Data.Typeable
import Prelude.Unicode

data DescribeStream
  = DescribeStream
  { _dstExclusiveStartShardId ∷ !(Maybe ShardId)
    -- ^ The shard id of the first item this operation will evalute; see also
    -- '_sdLastEvaluatedShardId'.

  , _dstLimit ∷ !(Maybe Int)
    -- ^ The maximum number of shard objects to return.

  , _dstStreamArn ∷ !Arn
    -- ^ The ARN of the stream to be described.

  } deriving (Eq, Ord, Show, Read, Typeable)

-- | A basic 'DescribeStream' request for a given stream id.
--
-- @
-- myRequest =
--   'describeStream' myStream
--     & 'dstLimit' ?~ 3
-- @
--
describeStream
  ∷ Arn
  → DescribeStream
describeStream streamArn = DescribeStream
  { _dstExclusiveStartShardId = Nothing
  , _dstLimit = Nothing
  , _dstStreamArn = streamArn
  }

instance ToJSON DescribeStream where
  toJSON DescribeStream{..} = object
    [ "ExclusiveStartShardId" .= _dstExclusiveStartShardId
    , "Limit" .= _dstLimit
    , "StreamArn".= _dstStreamArn
    ]

instance FromJSON DescribeStream where
  parseJSON =
    withObject "DescribeStream" $ \o →
      pure DescribeStream
        ⊛ o .:? "ExclusiveStartShardId"
        ⊛ o .:? "Limit"
        ⊛ o .: "StreamArn"

-- | A lens for '_dstExclusiveStartShardId'.
--
-- @
-- 'dstExclusiveStartShardId' ∷ Lens' 'DescribeStream' ('Maybe' 'ShardId')
-- @
--
dstExclusiveStartShardId
  ∷ Functor f
  ⇒ (Maybe ShardId → f (Maybe ShardId))
  → DescribeStream
  → f DescribeStream
dstExclusiveStartShardId i DescribeStream{..} =
  (\_dstExclusiveStartShardId → DescribeStream{..})
    <$> i _dstExclusiveStartShardId
{-# INLINE dstExclusiveStartShardId #-}

-- | A lens for '_dstLimit'.
--
-- @
-- 'dstLimit' ∷ Lens' 'DescribeStream' ('Maybe' 'Int')
-- @
--
dstLimit
  ∷ Functor f
  ⇒ (Maybe Int → f (Maybe Int))
  → DescribeStream
  → f DescribeStream
dstLimit i DescribeStream{..} =
  (\_dstLimit → DescribeStream{..})
    <$> i _dstLimit
{-# INLINE dstLimit #-}

-- | A lens for '_dstStreamArn'.
--
-- @
-- 'dstStreamArn' ∷ Lens' 'DescribeStream' 'StreamArn'
-- @
--
dstStreamArn
  ∷ Functor f
  ⇒ (Arn → f Arn)
  → DescribeStream
  → f DescribeStream
dstStreamArn i DescribeStream{..} =
  (\_dstStreamArn → DescribeStream{..})
    <$> i _dstStreamArn
{-# INLINE dstStreamArn #-}

data DescribeStreamResponse
  = DescribeStreamResponse
  { _dstrStreamDescription ∷ !StreamDescription
  } deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON DescribeStreamResponse where
  toJSON DescribeStreamResponse{..} = object
    [ "StreamDescription" .= _dstrStreamDescription
    ]

instance FromJSON DescribeStreamResponse where
  parseJSON =
    withObject "DescribeStreamResponse" $ \o →
      DescribeStreamResponse
        <$> o .: "StreamDescription"


-- | A lens for '_dstrStreamDescription'.
--
-- @
-- 'dstrStreamDescription' ∷ Lens' 'DescribeStreamResponse' 'StreamDescription'
-- @
--
dstrStreamDescription
  ∷ Functor f
  ⇒ (StreamDescription → f StreamDescription)
  → DescribeStreamResponse
  → f DescribeStreamResponse
dstrStreamDescription i DescribeStreamResponse{..} =
  (\_dstrStreamDescription → DescribeStreamResponse{..})
    <$> i _dstrStreamDescription
{-# INLINE dstrStreamDescription #-}

instance ResponseConsumer r DescribeStreamResponse where
  type ResponseMetadata DescribeStreamResponse = StreamsMetadata
  responseConsumer _ = streamsResponseConsumer

instance SignQuery DescribeStream where
  type ServiceConfiguration DescribeStream = StreamsConfiguration
  signQuery cmd = streamsSignQuery StreamsQuery
    { _stqAction = ActionDescribeStream
    , _stqBody = encode cmd
    }

instance Transaction DescribeStream DescribeStreamResponse

instance AsMemoryResponse DescribeStreamResponse where
  type MemoryResponse DescribeStreamResponse = DescribeStreamResponse
  loadToMemory = return

instance ListResponse DescribeStreamResponse Shard where
  listResponse = _sdShards ∘ _dstrStreamDescription

instance IteratedTransaction DescribeStream DescribeStreamResponse where
  nextIteratedRequest req@DescribeStream{..} DescribeStreamResponse{..} = do
    let StreamDescription{..} = _dstrStreamDescription
    lastEvaluatedShardId ← _sdLastEvaluatedShardId
    return req
      { _dstExclusiveStartShardId = Just lastEvaluatedShardId
      }

