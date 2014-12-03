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
{-# LANGUAGE UnicodeSyntax #-}

module Aws.DynamoDb.Streams.Core
( StreamsAction(..)
, streamsActionToText
, parseStreamsAction
, streamsServiceEndpoint
, StreamsMetadata(..)
, smAmzId2
, smRequestId
, StreamsConfiguration(..)
, stcRegion
, StreamsQuery(..)
, stqAction
, stqBody
, streamsSignQuery
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
import Aws.SignatureV4

import Control.Applicative
import Control.Applicative.Unicode
import Control.Exception
import Control.Monad.Trans
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkLbs)
import Data.IORef
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
  = DescribeStream
  | GetRecords
  | GetShardIterator
  | ListStreams
  deriving (Eq, Ord, Enum, Bounded, Typeable, Read, Show)

streamsActionToText
  ∷ IsString s
  ⇒ StreamsAction
  → s
streamsActionToText = \case
  DescribeStream → "DescribeStream"
  GetRecords → "GetRecords"
  GetShardIterator → "GetShardIterator"
  ListStreams → "ListStreams"

parseStreamsAction
  ∷ P.CharParsing m
  ⇒ m StreamsAction
parseStreamsAction = P.choice parsers <?> "StreamsAction"
  where
    actionToParser a =
      a <$ P.text (streamsActionToText a)

    parsers = actionToParser <$>
      [ DescribeStream
      , GetRecords
      , GetShardIterator
      , ListStreams
      ]

instance AwsType StreamsAction where
  toText = streamsActionToText
  parse = parseStreamsAction

streamsServiceEndpoint
  ∷ Region
  → B8.ByteString
streamsServiceEndpoint = \case
  UsEast1 → "http://mkto-sj010128.com/HK0030i0yIn6mAa03gV00SK"
  EuWest1 → "http://mkto-sj010128.com/SV0A3K0630a0yKI0g00mioT"
  region → error $ "Unsupported region: " ⊕ show region

data StreamsMetadata
  = StreamsMetadata
  { _smAmzId2 ∷ !(Maybe T.Text)
  , _smRequestId ∷ !(Maybe T.Text)
  } deriving (Eq, Show)

instance Loggable StreamsMetadata where
  toLogText StreamsMetadata{..} =
    "DynamoDb Streams: request ID="
      ⊕ fromMaybe "<none>" _smRequestId
      ⊕ ", x-amz-id-2="
      ⊕ fromMaybe "<none>" _smAmzId2

instance Monoid StreamsMetadata where
  mempty = StreamsMetadata
    { _smAmzId2 = Nothing
    , _smRequestId = Nothing
    }

  sm `mappend` sm' = StreamsMetadata
    { _smAmzId2 = _smAmzId2 sm <|> _smAmzId2 sm'
    , _smRequestId = _smRequestId sm <|> _smRequestId sm'
    }

-- | A lens for '_smAmzId2'.
--
-- @
-- 'smAmzId2' ∷ Lens' 'StreamsMetadata' ('Maybe' 'T.Text')
-- @
--
smAmzId2
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → StreamsMetadata
  → f StreamsMetadata
smAmzId2 i StreamsMetadata{..} =
  (\_smAmzId2 → StreamsMetadata{..})
    <$> i _smAmzId2

-- | A lens for '_smRequestId'.
--
-- @
-- 'smRequestId' ∷ Lens' 'StreamsMetadata' ('Maybe' 'T.Text')
-- @
--
smRequestId
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → StreamsMetadata
  → f StreamsMetadata
smRequestId i StreamsMetadata{..} =
  (\_smRequestId → StreamsMetadata{..})
    <$> i _smRequestId

data StreamsConfiguration qt
  = StreamsConfiguration
  { _stcRegion ∷ !Region
  } deriving (Eq, Show)

-- | A lens for '_stcRegion'
--
-- @
-- 'stcRegion' ∷ Lens' 'StreamsConfiguration' 'Region'
-- @
--
stcRegion
  ∷ Functor f
  ⇒ (Region → f Region)
  → (StreamsConfiguration qt)
  → f (StreamsConfiguration qt)
stcRegion i StreamsConfiguration{..} =
  StreamsConfiguration <$> i _stcRegion

data StreamsQuery
  = StreamsQuery
  { _stqAction ∷ !StreamsAction
  , _stqBody ∷ !(Maybe B.ByteString)
  } deriving (Eq, Show)

-- | A lens for '_stqAction'
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

-- | A lens for '_stqBody'
--
-- @
-- 'stqBody' ∷ Lens' 'StreamsQuery' ('Maybe' 'B.ByteString')
-- @
--
stqBody
  ∷ Functor f
  ⇒ (Maybe B.ByteString → f (Maybe B.ByteString))
  → StreamsQuery
  → f StreamsQuery
stqBody i StreamsQuery{..} =
  (\_stqBody → StreamsQuery{..})
    <$> i _stqBody

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
    , sqProtocol = HTTPS
    , sqHost = host
    , sqPort = port
    , sqPath = BB.toByteString $ HTTP.encodePathSegments path
    , sqQuery = reqQuery
    , sqDate = Nothing
    , sqAuthorization = authorization
    , sqContentType = contentType
    , sqContentMd5 = Nothing
    , sqAmzHeaders = amzHeaders
    , sqOtherHeaders = [] -- we put everything into amzHeaders
    , sqBody = HTTP.RequestBodyBS <$> _stqBody
    , sqStringToSign = mempty
    }
  where
    path = []
    reqQuery = []
    host = streamsServiceEndpoint $ _stcRegion
    headers = [("host", host), streamsTargetHeader _stqAction]
    port = 443
    contentType = Just "application/x-amz-json-1.1"

    amzHeaders = filter ((/= "Authorization") ∘ fst) sig
    authorization = return <$> lookup "authorization" sig
    convertCredentials Credentials{..} = SignatureV4Credentials
      { sigV4AccessKeyId = accessKeyID
      , sigV4SecretAccessKey = secretAccessKey
      , sigV4SigningKeys = v4SigningKeys
      }

    sig =
      either error id $
        signPostRequest
          (convertCredentials $ signatureCredentials sigData)
          _stcRegion
          ServiceNamespaceKinesis
          (signatureTime sigData)
          "POST"
          path
          reqQuery
          headers
          (fromMaybe "" _stqBody)


data StreamsErrorResponseData
  = StreamsErrorResponseData
  { _sterdErrorCode ∷ !T.Text
  , _sterdErrorMessage ∷ !T.Text
  } deriving (Eq, Show, Typeable)

-- | A lens for '_sterdErrorCode'
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

-- | A lens for '_sterdErrorMessage'
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

data StreamsErrorResponse
  = StreamsResponseJsonError T.Text
  | StreamsErrorResponse StreamsErrorResponseData
  | StreamsOtherError StreamsOtherErrorData
  deriving (Eq, Show, Typeable)

-- | A prism for 'StreamsResponseJsonError'
--
-- @
-- '_StreamsResponseJsonError' ∷ Prism' 'StreamsErrorResponse' 'T.Text'
-- @
_StreamsResponseJsonError
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p T.Text (f T.Text)
  → p StreamsErrorResponse (f StreamsErrorResponse)
_StreamsResponseJsonError =
  dimap to fro ∘ right'
    where
      to = \case
        StreamsResponseJsonError e → Right e
        e → Left e
      fro = either pure (fmap StreamsResponseJsonError)

-- | A prism for 'StreamsErrorResponse'
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

-- | A prism for 'StreamsOtherError'
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
    Left err → throwM ∘ StreamsResponseJsonError $ T.pack err
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
    { _smAmzId2 = amzId2
    , _smRequestId = requestId
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
        Left e → throwM ∘ StreamsResponseJsonError $ T.pack e
        Right a → do
          throwM (a ∷ StreamsErrorResponse)

