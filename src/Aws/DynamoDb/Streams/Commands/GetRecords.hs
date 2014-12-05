-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.DynamoDb.Streams.Commands.GetRecords
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
module Aws.DynamoDb.Streams.Commands.GetRecords
( -- * Request
  GetRecords(..)
, getRecords
  -- ** Lenses
, grShardIterator
, grLimit

  -- * Response
, GetRecordsResponse(..)
  -- ** Lenses
, grrRecords
, grrNextShardIterator
) where

import Aws.Core
import Aws.DynamoDb.Streams.Core
import Aws.DynamoDb.Streams.Types
import Control.Applicative
import Control.Applicative.Unicode
import Data.Aeson
import Data.Typeable

data GetRecords
  = GetRecords
  { _grShardIterator ∷ !ShardIterator
  , _grLimit ∷ !(Maybe Int)
  } deriving (Eq, Ord, Show, Read, Typeable)

-- | A basic 'GetRecords' request for a given shard iterator.
--
-- @
-- myRequest = getRecords it & grLimit ?~ 42
-- @
--
getRecords
  ∷ ShardIterator
  → GetRecords
getRecords it = GetRecords
  { _grShardIterator = it
  , _grLimit = Nothing
  }

instance ToJSON GetRecords where
  toJSON GetRecords{..} = object
    [ "ShardIterator" .= _grShardIterator
    , "Limit" .= _grLimit
    ]

instance FromJSON GetRecords where
  parseJSON =
    withObject "GetRecords" $ \o →
      pure GetRecords
        ⊛ o .: "ShardIterator"
        ⊛ o .:? "Limit"

-- | A lens for '_grShardIterator'.
--
-- @
-- grShardIterator ∷ Lens' 'GetRecords' 'ShardIterator'
-- @
--
grShardIterator
  ∷ Functor f
  ⇒ (ShardIterator → f ShardIterator)
  → GetRecords
  → f GetRecords
grShardIterator i GetRecords{..} =
  (\_grShardIterator → GetRecords{..})
    <$> i _grShardIterator
{-# INLINE grShardIterator #-}

-- | A lens for '_grLimit'.
--
-- @
-- grLimit ∷ Lens' 'GetRecords' ('Maybe' 'Int')
-- @
--
grLimit
  ∷ Functor f
  ⇒ (Maybe Int → f (Maybe Int))
  → GetRecords
  → f GetRecords
grLimit i GetRecords{..} =
  (\_grLimit → GetRecords{..})
    <$> i _grLimit
{-# INLINE grLimit #-}

data GetRecordsResponse
  = GetRecordsResponse
  { _grrNextShardIterator ∷ !(Maybe ShardIterator)
  , _grrRecords ∷ ![Record]
  } deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON GetRecordsResponse where
  toJSON GetRecordsResponse{..} = object
    [ "NextShardIterator" .= _grrNextShardIterator
    , "Records" .= _grrRecords
    ]

instance FromJSON GetRecordsResponse where
  parseJSON =
    withObject "GetRecordsResponse" $ \o →
      pure GetRecordsResponse
        ⊛ o .:? "NextShardIterator"
        ⊛ o .: "Records"

-- | A lens for '_grrRecords'.
--
-- @
-- grrRecords ∷ Lens' 'GetRecordsResponse' ['Record']
-- @
--
grrRecords
  ∷ Functor f
  ⇒ ([Record] → f [Record])
  → GetRecordsResponse
  → f GetRecordsResponse
grrRecords i GetRecordsResponse{..} =
  (\_grrRecords → GetRecordsResponse{..})
    <$> i _grrRecords
{-# INLINE grrRecords #-}

-- | A lens for '_grrNextShardIterator'.
--
-- @
-- grrNextShardIterator ∷ Lens' 'GetRecordsResponse' ('Maybe' 'ShardIterator')
-- @
--
grrNextShardIterator
  ∷ Functor f
  ⇒ (Maybe ShardIterator → f (Maybe ShardIterator))
  → GetRecordsResponse
  → f GetRecordsResponse
grrNextShardIterator i GetRecordsResponse{..} =
  (\_grrNextShardIterator → GetRecordsResponse{..})
    <$> i _grrNextShardIterator
{-# INLINE grrNextShardIterator #-}

instance ResponseConsumer r GetRecordsResponse where
  type ResponseMetadata GetRecordsResponse = StreamsMetadata
  responseConsumer _ = streamsResponseConsumer

instance SignQuery GetRecords where
  type ServiceConfiguration GetRecords = StreamsConfiguration
  signQuery cmd = streamsSignQuery StreamsQuery
    { _stqAction = ActionGetRecords
    , _stqBody = encode cmd
    }

instance Transaction GetRecords GetRecordsResponse

instance AsMemoryResponse GetRecordsResponse where
  type MemoryResponse GetRecordsResponse = GetRecordsResponse
  loadToMemory = return

instance ListResponse GetRecordsResponse Record where
  listResponse = _grrRecords

instance IteratedTransaction GetRecords GetRecordsResponse where
  nextIteratedRequest req@GetRecords{..} GetRecordsResponse{..} = do
    nextShardIterator ← _grrNextShardIterator
    return req
      { _grShardIterator = nextShardIterator
      }
