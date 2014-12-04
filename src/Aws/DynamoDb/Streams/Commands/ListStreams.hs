-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.DynamoDb.Streams.Commands.ListStreams
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
module Aws.DynamoDb.Streams.Commands.ListStreams
( -- * Request
  ListStreams(..)
, listStreams
  -- ** Lenses
, lstExclusiveStartStreamId
, lstLimit
, lstTableName

  -- * Response
, ListStreamsResponse(..)

  -- ** Lenses
, lstrLastEvalutedStreamId
, lstrStreamIds
) where

import Aws.Core
import Aws.DynamoDb.Streams.Core
import Aws.DynamoDb.Streams.Types

import Control.Applicative
import Control.Applicative.Unicode
import Data.Aeson
import Data.Monoid
import Data.Typeable

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

data ListStreams
  = ListStreams
  { _lstExclusiveStartStreamId ∷ !(Maybe StreamId)
    -- ^ The stream id of the first item that this operation will evaluate;
    -- also see '_lstrLastEvalutedStreamId'.

  , _lstLimit ∷ !(Maybe Int)
    -- ^ The maximum number of streams to return.

  , _lstTableName ∷ !(Maybe T.Text)
    -- ^ If this parameter is provided, then only the streams associated with
    -- this table are returned.

  } deriving (Eq, Ord, Show, Read, Typeable)


instance ToJSON ListStreams where
  toJSON ListStreams{..} = object
    [ "ExclusiveStartStreamId" .= _lstExclusiveStartStreamId
    , "Limit" .= _lstLimit
    , "TableName" .= _lstTableName
    ]

instance FromJSON ListStreams where
  parseJSON =
    withObject "ListStreams" $ \o →
      pure ListStreams
        ⊛ o .:? "ExclusiveStartStreamId"
        ⊛ o .:? "Limit"
        ⊛ o .:? "TableName"


instance Monoid ListStreams where
  mempty = listStreams
  ls `mappend` ls' = ListStreams
    { _lstExclusiveStartStreamId = _lstExclusiveStartStreamId ls <|> _lstExclusiveStartStreamId ls'
    , _lstLimit = _lstLimit ls <|> _lstLimit ls'
    , _lstTableName = _lstTableName ls <|> _lstTableName ls'
    }

-- | An empty 'ListStreams' request.
--
-- @
-- myRequest = 'listStreams' & 'lstTableName' ?~ "UsersTable"
-- @
--
listStreams ∷ ListStreams
listStreams = ListStreams
  { _lstExclusiveStartStreamId = Nothing
  , _lstLimit = Nothing
  , _lstTableName = Nothing
  }

-- | A lens for '_lstExclusiveStartStreamId'.
--
-- @
-- lstExclusiveStartStreamId ∷ Lens' 'ListStreams' ('Maybe' 'StreamId')
-- @
--
lstExclusiveStartStreamId
  ∷ Functor f
  ⇒ (Maybe StreamId → f (Maybe StreamId))
  → ListStreams
  → f ListStreams
lstExclusiveStartStreamId i ListStreams{..} =
  (\_lstExclusiveStartStreamId → ListStreams{..})
    <$> i _lstExclusiveStartStreamId

-- | A lens for '_lstlimit'.
--
-- @
-- lstLimit ∷ Lens' 'ListStreams' ('Maybe' 'Int')
-- @
--
lstLimit
  ∷ Functor f
  ⇒ (Maybe Int → f (Maybe Int))
  → ListStreams
  → f ListStreams
lstLimit i ListStreams{..} =
  (\_lstLimit → ListStreams{..})
    <$> i _lstLimit

-- | A lens for '_lstTableName'.
--
-- @
-- lstTableName ∷ Lens' 'ListStreams' ('Maybe' 'T.Text')
-- @
--
lstTableName
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → ListStreams
  → f ListStreams
lstTableName i ListStreams{..} =
  (\_lstTableName → ListStreams{..})
    <$> i _lstTableName


data ListStreamsResponse
  = ListStreamsResponse
  { _lstrLastEvalutedStreamId ∷ !(Maybe StreamId)
    -- ^ When empty, this indicates that there are no more streams to be
    -- retrieved.

  , _lstrStreamIds ∷ ![StreamId]
    -- ^ A list of stream ids associated with the current account and endpoint.
  } deriving (Eq, Ord, Read, Show, Typeable)

instance ToJSON ListStreamsResponse where
  toJSON ListStreamsResponse{..} = object
    [ "LastEvaluatedStreamId" .= _lstrLastEvalutedStreamId
    , "StreamIds" .= _lstrStreamIds
    ]

instance FromJSON ListStreamsResponse where
  parseJSON =
    withObject "ListStreamsResponse" $ \o →
      pure ListStreamsResponse
        ⊛ o .:? "LastEvaluatedStreamId"
        ⊛ o .:? "StreamIds" .!= []

-- | A lens for '_lstrLastEvalutedStreamId'.
--
-- @
-- lstrLastEvalutedStreamId ∷ Lens' 'ListStreamsResponse' ('Maybe' 'StreamId')
-- @
--
lstrLastEvalutedStreamId
  ∷ Functor f
  ⇒ (Maybe StreamId → f (Maybe StreamId))
  → ListStreamsResponse
  → f ListStreamsResponse
lstrLastEvalutedStreamId i ListStreamsResponse{..} =
  (\_lstrLastEvalutedStreamId → ListStreamsResponse{..})
    <$> i _lstrLastEvalutedStreamId

-- | A lens for '_lstrStreamIds'.
--
-- @
-- lstrStreamIds ∷ Lens' 'ListStreamsResponse' ['StreamId']
-- @
--
lstrStreamIds
  ∷ Functor f
  ⇒ ([StreamId] → f [StreamId])
  → ListStreamsResponse
  → f ListStreamsResponse
lstrStreamIds i ListStreamsResponse{..} =
  (\_lstrStreamIds → ListStreamsResponse{..})
    <$> i _lstrStreamIds

instance ResponseConsumer r ListStreamsResponse where
  type ResponseMetadata ListStreamsResponse = StreamsMetadata
  responseConsumer _ = streamsResponseConsumer

instance SignQuery ListStreams where
  type ServiceConfiguration ListStreams = StreamsConfiguration
  signQuery cmd = streamsSignQuery StreamsQuery
    { _stqAction = ActionListStreams
    , _stqBody = encode cmd
    }

instance Transaction ListStreams ListStreamsResponse

instance AsMemoryResponse ListStreamsResponse where
  type MemoryResponse ListStreamsResponse = ListStreamsResponse
  loadToMemory = return

instance ListResponse ListStreamsResponse StreamId where
  listResponse = _lstrStreamIds

instance IteratedTransaction ListStreams ListStreamsResponse where
  nextIteratedRequest req@ListStreams{..} ListStreamsResponse{..} = do
    lastEvaluatedStreamId ← _lstrLastEvalutedStreamId
    return req
      { _lstExclusiveStartStreamId = Just lastEvaluatedStreamId
      }
