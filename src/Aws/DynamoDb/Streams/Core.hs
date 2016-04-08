-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.DynamoDb.Streams.Core
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache-2.0
--
module Aws.DynamoDb.Streams.Core
( StreamsAction(..)
, streamsActionToText
, parseStreamsAction
, streamsServiceEndpoint
, StreamsMetadata(..)
, stmAmzId2
, stmRequestId
, StreamsConfiguration(..)
, stcRegion
, StreamsQuery(..)
, stqAction
, stqBody
, streamsSignQuery
, StreamsResponseJsonErrorData(..)
, srjedMessage
, srjedJSON
, StreamsErrorResponse(..)
, _StreamsResponseJsonError
, _StreamsErrorResponse
, _StreamsOtherError
, StreamsErrorResponseData(..)
, sterdErrorCode
, sterdErrorMessage
, StreamsOtherErrorData(..)
, stoeStatus
, stoeMessage
, jsonResponseConsumer
, errorResponseConsumer
, streamsResponseConsumer
) where

import Aws.Core
import Aws.General

import Control.Applicative
import Control.Applicative.Unicode
import Control.Exception
import Control.Monad.Trans
import Crypto.Hash
import Data.Byteable
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkLbs)
import Data.Function (on)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Profunctor
import Data.String
import Data.Typeable
import Control.Monad.Trans.Resource (throwM)
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP
import Prelude.Unicode
import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P
import Text.Parser.Combinators ((<?>))

streamsTargetVersion ∷ IsString a ⇒ a
streamsTargetVersion = "DynamoDBStreams_20120810"

data StreamsAction
  = ActionDescribeStream
  | ActionGetRecords
  | ActionGetShardIterator
  | ActionListStreams
  deriving (Eq, Ord, Enum, Bounded, Typeable, Read, Show)

-- | Render a 'StreamsAction' as a string for use in an AWS request.
--
streamsActionToText
  ∷ IsString s
  ⇒ StreamsAction
  → s
streamsActionToText = \case
  ActionDescribeStream → "DescribeStream"
  ActionGetRecords → "GetRecords"
  ActionGetShardIterator → "GetShardIterator"
  ActionListStreams → "ListStreams"

-- | Parse a 'StreamsAction'; this is the inverse of 'streamsActionToText'.
--
parseStreamsAction
  ∷ P.CharParsing m
  ⇒ m StreamsAction
parseStreamsAction = P.choice parsers <?> "StreamsAction"
  where
    actionToParser a =
      a <$ P.text (streamsActionToText a)

    parsers = actionToParser <$>
      [ ActionDescribeStream
      , ActionGetRecords
      , ActionGetShardIterator
      , ActionListStreams
      ]

instance AwsType StreamsAction where
  toText = streamsActionToText
  parse = parseStreamsAction

streamsServiceEndpoint
  ∷ Region
  → B8.ByteString
streamsServiceEndpoint = \case
  ApNortheast1 → "streams.dynamodb.ap-northeast-1.amazonaws.com"
  ApSoutheast1 → "streams.dynamodb.ap-southeast-1.amazonaws.com"
  ApSoutheast2 → "streams.dynamodb.ap-southeast-2.amazonaws.com"
  EuWest1 → "streams.dynamodb.eu-west-1.amazonaws.com"
  SaEast1 → "streams.dynamodb.sa-east-1.amazonaws.com"
  UsEast1 → "streams.dynamodb.us-east-1.amazonaws.com"
  UsWest1 → "streams.dynamodb.us-west-1.amazonaws.com"
  UsWest2 → "streams.dynamodb.us-west-2.amazonaws.com"
  CustomEndpoint u p → T.encodeUtf8 u <> ":" <> fromString (show p)

data StreamsMetadata
  = StreamsMetadata
  { _stmAmzId2 ∷ !(Maybe T.Text)
  , _stmRequestId ∷ !(Maybe T.Text)
  } deriving (Eq, Show)

instance Loggable StreamsMetadata where
  toLogText StreamsMetadata{..} =
    "DynamoDb Streams: request ID="
      ⊕ fromMaybe "<none>" _stmRequestId
      ⊕ ", x-amz-id-2="
      ⊕ fromMaybe "<none>" _stmAmzId2

instance Monoid StreamsMetadata where
  mempty = StreamsMetadata
    { _stmAmzId2 = Nothing
    , _stmRequestId = Nothing
    }

  sm `mappend` sm' = StreamsMetadata
    { _stmAmzId2 = _stmAmzId2 sm <|> _stmAmzId2 sm'
    , _stmRequestId = _stmRequestId sm <|> _stmRequestId sm'
    }

-- | A lens for '_stmAmzId2'.
--
-- @
-- 'stmAmzId2' ∷ Lens' 'StreamsMetadata' ('Maybe' 'T.Text')
-- @
--
stmAmzId2
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → StreamsMetadata
  → f StreamsMetadata
stmAmzId2 i StreamsMetadata{..} =
  (\_stmAmzId2 → StreamsMetadata{..})
    <$> i _stmAmzId2
{-# INLINE stmAmzId2 #-}

-- | A lens for '_stmRequestId'.
--
-- @
-- 'stmRequestId' ∷ Lens' 'StreamsMetadata' ('Maybe' 'T.Text')
-- @
--
stmRequestId
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → StreamsMetadata
  → f StreamsMetadata
stmRequestId i StreamsMetadata{..} =
  (\_stmRequestId → StreamsMetadata{..})
    <$> i _stmRequestId
{-# INLINE stmRequestId #-}

data StreamsConfiguration qt
  = StreamsConfiguration
  { _stcRegion ∷ !Region
  } deriving (Eq, Show)

-- | A lens for '_stcRegion'.
--
-- @
-- 'stcRegion' ∷ Lens' 'StreamsConfiguration' 'Region'
-- @
--
stcRegion
  ∷ Functor f
  ⇒ (Region → f Region)
  → StreamsConfiguration qt
  → f (StreamsConfiguration qt)
stcRegion i StreamsConfiguration{..} =
  (\_stcRegion → StreamsConfiguration{..})
    <$> i _stcRegion
{-# INLINE stcRegion #-}

data StreamsQuery
  = StreamsQuery
  { _stqAction ∷ !StreamsAction
  , _stqBody ∷ !LB.ByteString
  } deriving (Eq, Show)

-- | A lens for '_stqAction'.
--
-- @
-- 'stqAction' ∷ Lens' 'StreamsQuery' 'StreamsAction'
-- @
--
stqAction
  ∷ Functor f
  ⇒ (StreamsAction → f StreamsAction)
  → StreamsQuery
  → f StreamsQuery
stqAction i StreamsQuery{..} =
  (\_stqAction → StreamsQuery{..})
    <$> i _stqAction
{-# INLINE stqAction #-}

-- | A lens for '_stqBody'.
--
-- @
-- 'stqBody' ∷ Lens' 'StreamsQuery' 'LB.ByteString'
-- @
--
stqBody
  ∷ Functor f
  ⇒ (LB.ByteString → f LB.ByteString)
  → StreamsQuery
  → f StreamsQuery
stqBody i StreamsQuery{..} =
  (\_stqBody → StreamsQuery{..})
    <$> i _stqBody
{-# INLINE stqBody #-}

streamsTargetHeader
  ∷ StreamsAction
  → HTTP.Header
streamsTargetHeader a =
  ( "X-Amz-Target"
  , streamsTargetVersion ⊕ "." ⊕ toText a
  )

-- | Creates a signed query.
--
-- Uses AWS Signature V4. All requests are POST requests
-- with the signature placed in an HTTP header
--
streamsSignQuery
  ∷ StreamsQuery
  → StreamsConfiguration qt
  → SignatureData
  → SignedQuery
streamsSignQuery StreamsQuery{..} StreamsConfiguration{..} sigData = SignedQuery
  { sqMethod = Post
  , sqProtocol = HTTP
  , sqHost = host
  , sqPort = 80
  , sqPath = "/"
  , sqQuery = []
  , sqDate = Just $ signatureTime sigData
  , sqAuthorization = Just auth
  , sqContentType = Just "application/x-amz-json-1.0"
  , sqContentMd5 = Nothing
  , sqAmzHeaders = amzHeaders ⊕ maybe [] pure securityTokenHeader
  , sqOtherHeaders = []
  , sqBody = Just $ HTTP.RequestBodyLBS _stqBody
  , sqStringToSign = canonicalRequest
  }
    where
      credentials = signatureCredentials sigData
      host = streamsServiceEndpoint _stcRegion
      sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sigData

      -- for some reason AWS doesn't want the x-amz-security-token in the canonical request
      amzHeaders =
        [ ("x-amz-date", sigTime)
        , streamsTargetHeader _stqAction
        ]

      securityTokenHeader =
        ("x-amz-security-token",)
          <$> iamToken credentials

      canonicalHeaders =
        sortBy (compare `on` fst) $
          amzHeaders ⊕
            [ ("host", host)
            , ("content-type", "application/x-amz-json-1.0")
            ]

      canonicalRequest =
        let bodyHash = B16.encode $ toBytes (hashlazy _stqBody :: Digest SHA256)
            headers =
              flip fmap canonicalHeaders $ \(a,b) →
                [ CI.foldedCase a
                , ":"
                , b
                ]
        in
          B.concat ∘ intercalate ["\n"] $
            [ [ "POST" ]
            , [ "/" ]
            , [] -- query string
            ] ⊕ headers ⊕
              [ [] -- end headers
              , intersperse ";" ((CI.foldedCase ∘ fst) <$> canonicalHeaders)
              , [ bodyHash ]
              ]

      auth =
        authorizationV4
          sigData
          HmacSHA256
          (regionToText _stcRegion)
          "dynamodb"
          "content-type;host;x-amz-date;x-amz-target"
          canonicalRequest


data StreamsErrorResponseData
  = StreamsErrorResponseData
  { _sterdErrorCode ∷ !T.Text
  , _sterdErrorMessage ∷ !T.Text
  } deriving (Eq, Show, Typeable)

-- | A lens for '_sterdErrorCode'.
--
-- @
-- 'sterdErrorCode' ∷ Lens' 'StreamsErrorResponseData' 'T.Text'
-- @
--
sterdErrorCode
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → StreamsErrorResponseData
  → f StreamsErrorResponseData
sterdErrorCode i StreamsErrorResponseData{..} =
  (\_sterdErrorCode → StreamsErrorResponseData{..})
    <$> i _sterdErrorCode
{-# INLINE sterdErrorCode #-}

-- | A lens for '_sterdErrorMessage'.
--
-- @
-- 'sterdErrorMessage' ∷ Lens' 'StreamsErrorResponseData' 'T.Text'
-- @
--
sterdErrorMessage
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → StreamsErrorResponseData
  → f StreamsErrorResponseData
sterdErrorMessage i StreamsErrorResponseData{..} =
  (\_sterdErrorMessage → StreamsErrorResponseData{..})
    <$> i _sterdErrorMessage
{-# INLINE sterdErrorMessage #-}

data StreamsOtherErrorData
  = StreamsOtherErrorData
  { _stoeStatus ∷ !HTTP.Status
  , _stoeMessage ∷ !T.Text
  } deriving (Eq, Show, Typeable)

-- | A lens for '_stoeStatus'
--
-- @
-- 'stoeStatus' ∷ Lens' 'StreamsOtherErrorData' 'HTTP.Status'
-- @
--
stoeStatus
  ∷ Functor f
  ⇒ (HTTP.Status → f HTTP.Status)
  → StreamsOtherErrorData
  → f StreamsOtherErrorData
stoeStatus i StreamsOtherErrorData{..} =
  (\_stoeStatus → StreamsOtherErrorData{..})
    <$> i _stoeStatus
{-# INLINE stoeStatus #-}

-- | A lens for '_stoeMessage'
--
-- @
-- 'stoeMessage' ∷ Lens' 'StreamsOtherErrorData' 'T.Text'
-- @
--
stoeMessage
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → StreamsOtherErrorData
  → f StreamsOtherErrorData
stoeMessage i StreamsOtherErrorData{..} =
  (\_stoeMessage → StreamsOtherErrorData{..})
    <$> i _stoeMessage
{-# INLINE stoeMessage #-}

data StreamsResponseJsonErrorData
  = StreamsResponseJsonErrorData
  { _srjedMessage ∷ !T.Text
  , _srjedJSON ∷ !LB.ByteString
  } deriving (Eq, Show, Typeable)

-- | A lens for '_srjedMessage'.
--
-- @
-- 'srjedMessage' ∷ Lens' 'StreamsResponseJsonErrorData' 'T.Text'
-- @
srjedMessage
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → StreamsResponseJsonErrorData
  → f StreamsResponseJsonErrorData
srjedMessage i StreamsResponseJsonErrorData{..} =
  (\_srjedMessage → StreamsResponseJsonErrorData{..})
    <$> i _srjedMessage

-- | A lens for '_srjedJSON'.
--
-- @
-- 'srjedJSON' ∷ Lens' 'StreamsResponseJsonErrorData' 'LB.ByteString'
-- @
srjedJSON
  ∷ Functor f
  ⇒ (LB.ByteString → f LB.ByteString)
  → StreamsResponseJsonErrorData
  → f StreamsResponseJsonErrorData
srjedJSON i StreamsResponseJsonErrorData{..} =
  (\_srjedJSON → StreamsResponseJsonErrorData{..})
    <$> i _srjedJSON

data StreamsErrorResponse
  = StreamsResponseJsonError StreamsResponseJsonErrorData
  | StreamsErrorResponse StreamsErrorResponseData
  | StreamsOtherError StreamsOtherErrorData
  deriving (Eq, Show, Typeable)

-- | A prism for 'StreamsResponseJsonError'.
--
-- @
-- '_StreamsResponseJsonError' ∷ Prism' 'StreamsErrorResponse' 'StreamsResponseJsonErrorData'
-- @
_StreamsResponseJsonError
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p StreamsResponseJsonErrorData (f StreamsResponseJsonErrorData)
  → p StreamsErrorResponse (f StreamsErrorResponse)
_StreamsResponseJsonError =
  dimap to fro ∘ right'
    where
      to = \case
        StreamsResponseJsonError e → Right e
        e → Left e
      fro = either pure (fmap StreamsResponseJsonError)
{-# INLINE _StreamsResponseJsonError #-}

-- | A prism for 'StreamsErrorResponse'.
--
-- @
-- '_StreamsErrorResponse' ∷ Prism' 'StreamsErrorResponse' 'StreamsErrorResponseData'
-- @
_StreamsErrorResponse
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p StreamsErrorResponseData (f StreamsErrorResponseData)
  → p StreamsErrorResponse (f StreamsErrorResponse)
_StreamsErrorResponse =
  dimap to fro ∘ right'
    where
      to = \case
        StreamsErrorResponse e → Right e
        e → Left e
      fro = either pure (fmap StreamsErrorResponse)
{-# INLINE _StreamsErrorResponse #-}

-- | A prism for 'StreamsOtherError'.
--
-- @
-- '_StreamsOtherError' ∷ Prism' 'StreamsErrorResponse' 'StreamsOtherErrorData'
-- @
_StreamsOtherError
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p StreamsOtherErrorData (f StreamsOtherErrorData)
  → p StreamsErrorResponse (f StreamsErrorResponse)
_StreamsOtherError =
  dimap to fro ∘ right'
    where
      to = \case
        StreamsOtherError e → Right e
        e → Left e
      fro = either pure (fmap StreamsOtherError)
{-# INLINE _StreamsOtherError #-}

instance Exception StreamsErrorResponse

-- | This instance captures only the 'StreamsErrorResponse' constructor
--
instance FromJSON StreamsErrorResponse where
  parseJSON =
    withObject "StreamsErrorResponse" $ \o →
      fmap StreamsErrorResponse $
        pure StreamsErrorResponseData
          ⊛ o .: "__type"
          ⊛ o .: "message"


-- | Create a complete 'HTTPResponseConsumer' for response types with an
-- 'FromJSON' instance
--
jsonResponseConsumer
  ∷ FromJSON α
  ⇒ HTTPResponseConsumer α
jsonResponseConsumer res = do
  doc ← HTTP.responseBody res $$+- sinkLbs
  case eitherDecode (if doc ≡ mempty then "{}" else doc) of
    Left e → throwM $ StreamsResponseJsonError StreamsResponseJsonErrorData
      { _srjedMessage = T.pack e
      , _srjedJSON = doc
      }
    Right v → return v


streamsResponseConsumer
  ∷ FromJSON a
  ⇒ IORef StreamsMetadata
  → HTTPResponseConsumer a
streamsResponseConsumer metadata resp = do

  let headerString = fmap T.decodeUtf8 ∘ flip lookup (HTTP.responseHeaders resp)
      amzId2 = headerString "x-amz-id-2"
      requestId = headerString "x-amz-request-id"

  liftIO $ tellMetadataRef metadata StreamsMetadata
    { _stmAmzId2 = amzId2
    , _stmRequestId = requestId
    }

  if HTTP.responseStatus resp ≥ HTTP.status400
    then errorResponseConsumer resp
    else jsonResponseConsumer resp

errorResponseConsumer ∷ HTTPResponseConsumer a
errorResponseConsumer resp = do
  doc ← HTTP.responseBody resp $$+- sinkLbs
  if HTTP.responseStatus resp ≡ HTTP.status400
    then kinesisError doc
    else throwM $ StreamsOtherError StreamsOtherErrorData
        { _stoeStatus = HTTP.responseStatus resp
        , _stoeMessage = T.decodeUtf8 $ LB.toStrict doc
        }
  where
    kinesisError doc =
      case eitherDecode doc of
        Left e → throwM $ StreamsResponseJsonError StreamsResponseJsonErrorData
          { _srjedMessage = T.pack e
          , _srjedJSON = doc
          }
        Right a → throwM (a ∷ StreamsErrorResponse)

