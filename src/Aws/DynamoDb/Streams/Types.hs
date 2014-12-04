-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Aws.DynamoDb.Streams.Types
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
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Copyright: Copyright (c) 2013-2014 PivotCloud, Inc.
-- License: Apache-2.0
--
module Aws.DynamoDb.Streams.Types
( -- * Sequence Numbers
  SequenceNumber
, SequenceNumberRange(..)
, sqnrStartingSequenceNumber
, sqnrEndingSequenceNumber

  -- * Shards
, ShardId
, Shard(..)
  -- ** Lenses
, shShardId
, shParentShardId
, shSequenceNumberRange

  -- * Attribute Values
, AttributeValue(..)
  -- ** Prisms
, _AVBin
, _AVBool
, _AVBinSet
, _AVList
, _AVMap
, _AVNum
, _AVNumSet
, _AVNull
, _AVString
, _AVStringSet

  -- * Key Schemas
  -- ** Key Types
, KeyType(..)
, keyTypeToText
  -- *** Prisms
, _KeyTypeHash
, _KeyTypeRange
  -- ** Key Schema Elements
, KeySchemaElement(..)
  -- *** Lenses
, kseAttributeName
, kseKeyType

  -- * Records
  -- ** Stream View Types
, StreamViewType(..)

  -- *** Prisms
, _StreamViewKeysOnly
, _StreamViewNewImage
, _StreamViewOldImage
, _StreamViewNewAndOldImages

  -- ** Stream Records
, StreamRecord(..)

  -- *** Lenses
, strKeys
, strNewImage
, strOldImage
, strSequenceNumber
, strSizeBytes
, strStreamViewType

  -- ** Event Names
, EventName(..)

  -- *** Prisms
, _EventInsert
, _EventModify
, _EventRemove

  -- ** Records
, Record(..)

  -- *** Lenses
, rAwsRegion
, rStreamRecord
, rEventId
, rEventName
, rEventSource
, rEventVersion


  -- * Stream Metadata
  -- ** Stream Statuses
, StreamStatus(..)

  --- *** Prisms
, _StatusEnabling
, _StatusEnabled
, _StatusDisabling
, _StatusDisabled

  -- ** Stream Description
, StreamId
, StreamDescription(..)
  -- *** Lenses
, sdCreationRequestDateTime
, sdKeySchema
, sdLastEvaluatedShardId
, sdShards
, sdStreamARN
, sdStreamId
, sdStreamStatus
, sdStreamViewType
, sdTableName
) where

import Aws.Core
import Aws.General
import Control.Applicative
import Control.Applicative.Unicode
import Control.Monad.Unicode
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Foldable (asum)
import qualified Data.Map as M
import Data.Monoid.Unicode
import Data.Profunctor
import Data.Scientific
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Traversable hiding (mapM)
import Data.Typeable
import Prelude.Unicode
import System.Locale

-- | Identifier for a shard.
--
-- Length constraints: @28 ≤ n ≤ 65@.
--
newtype ShardId
  = ShardId
  { _sidText ∷ T.Text
  } deriving (Eq, Ord, Typeable, Show, Read)

instance ToJSON ShardId where
  toJSON = toJSON ∘ _sidText

instance FromJSON ShardId where
  parseJSON =
    withText "ShardId" $
      pure ∘ ShardId

-- | Identifier for a record.
--
-- Length constraints: @21 ≤ n ≤ 40@
--
newtype SequenceNumber
  = SequenceNumber
  { _sqnText ∷ T.Text
  } deriving (Eq, Ord, Typeable, Show, Read)

instance ToJSON SequenceNumber where
  toJSON = toJSON ∘ _sqnText

instance FromJSON SequenceNumber where
  parseJSON =
    withText "SequenceNumber" $
      pure ∘ SequenceNumber

-- | The beginning and ending sequence numbers for the stream records contained
-- within a shard.
--
data SequenceNumberRange
  = SequenceNumberRange
  { _sqnrStartingSequenceNumber ∷ !(Maybe SequenceNumber)
  , _sqnrEndingSequenceNumber ∷ !(Maybe SequenceNumber)
  } deriving (Eq, Ord, Typeable, Show, Read)

-- | A lens for '_sqnrStartingSequenceNumber'.
--
-- @
-- 'sqnrStartingSequenceNumber' ∷ Lens' 'SequenceNumberRange' ('Maybe' 'SequenceNumber')
-- @
--
sqnrStartingSequenceNumber
  ∷ Functor f
  ⇒ (Maybe SequenceNumber → f (Maybe SequenceNumber))
  → SequenceNumberRange
  → f SequenceNumberRange
sqnrStartingSequenceNumber i SequenceNumberRange{..} =
  (\_sqnrStartingSequenceNumber → SequenceNumberRange{..})
    <$> i _sqnrStartingSequenceNumber
{-# INLINE sqnrStartingSequenceNumber #-}

-- | A lens for '_sqnrEndingSequenceNumber'.
--
-- @
-- 'sqnrEndingSequenceNumber' ∷ Lens' 'SequenceNumberRange' ('Maybe' 'SequenceNumber')
-- @
--
sqnrEndingSequenceNumber
  ∷ Functor f
  ⇒ (Maybe SequenceNumber → f (Maybe SequenceNumber))
  → SequenceNumberRange
  → f SequenceNumberRange
sqnrEndingSequenceNumber i SequenceNumberRange{..} =
  (\_sqnrEndingSequenceNumber → SequenceNumberRange{..})
    <$> i _sqnrEndingSequenceNumber
{-# INLINE sqnrEndingSequenceNumber #-}

instance ToJSON SequenceNumberRange where
  toJSON SequenceNumberRange{..} = object
    [ "StartingSequenceNumber" .= _sqnrStartingSequenceNumber
    , "EndingSequenceNumber" .= _sqnrEndingSequenceNumber
    ]

instance FromJSON SequenceNumberRange where
  parseJSON =
    withObject "SequenceNumberRange" $ \o →
      pure SequenceNumberRange
        ⊛ o .:? "StartingSequenceNumber"
        ⊛ o .:? "EndingSequenceNumber"

-- | A uniquely identified group of stream records within a stream.
--
data Shard
  = Shard
  { _shShardId ∷ !(Maybe ShardId)
    -- ^ The system-generated identifier for this shard
  , _shParentShardId ∷ !(Maybe ShardId)
    -- ^ The shard id of this shard's parent
  , _shSequenceNumberRange ∷ !(Maybe SequenceNumberRange)
  -- ^ The range of possible sequence numbers for this shard
  } deriving (Eq, Ord, Typeable, Show, Read)

instance ToJSON Shard where
  toJSON Shard{..} = object
    [ "ShardId" .= _shShardId
    , "ParentShardId" .= _shParentShardId
    , "SequenceNumberRange" .= _shSequenceNumberRange
    ]

instance FromJSON Shard where
  parseJSON =
    withObject "Shard" $ \o →
      pure Shard
        ⊛ o .:? "ShardId"
        ⊛ o .:? "ParentShardId"
        ⊛ o .:? "SequenceNumberRange"

-- | A lens for '_shShardId'.
--
-- @
-- 'shshardId' ∷ Lens' 'Shard' ('Maybe' 'ShardId')
-- @
--
shShardId
  ∷ Functor f
  ⇒ (Maybe ShardId → f (Maybe ShardId))
  → Shard
  → f Shard
shShardId i Shard{..} =
  (\_shShardId → Shard{..})
    <$> i _shShardId
{-# INLINE shShardId #-}

-- | A lens for '_shParentShardId'.
--
-- @
-- 'shParentShardId' ∷ Lens' 'Shard' ('Maybe' 'ShardId')
-- @
--
shParentShardId
  ∷ Functor f
  ⇒ (Maybe ShardId → f (Maybe ShardId))
  → Shard
  → f Shard
shParentShardId i Shard{..} =
  (\_shParentShardId → Shard{..})
    <$> i _shParentShardId
{-# INLINE shParentShardId #-}

-- | A lens for '_shSequenceNumberRange'.
--
-- @
-- 'shSequenceNumberRange' ∷ Lens' 'Shard' ('Maybe' 'SequenceNumberRange')
-- @
--
shSequenceNumberRange
  ∷ Functor f
  ⇒ (Maybe SequenceNumberRange → f (Maybe SequenceNumberRange))
  → Shard
  → f Shard
shSequenceNumberRange i Shard{..} =
  (\_shSequenceNumberRange → Shard{..})
    <$> i _shSequenceNumberRange
{-# INLINE shSequenceNumberRange #-}

-- | Represents the data for an attribute.
--
data AttributeValue
  = AVBin !B.ByteString
    -- ^ Binary data will automatically be base64 marshalled
  | AVBool !Bool
  | AVBinSet !(S.Set B.ByteString)
    -- ^ Binary data will automatically be base64 marshalled
  | AVList ![AttributeValue]
  | AVMap !(M.Map T.Text AttributeValue)
  | AVNum !Scientific
  | AVNumSet !(S.Set Scientific)
  | AVNull !Bool -- TODO: why is this 'Bool'?
  | AVString !T.Text
  | AVStringSet !(S.Set T.Text)
  deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON AttributeValue where
  toJSON = \case
    AVBin b → object ["B" .= T.decodeUtf8 (B64.encode b)]
    AVBinSet bs → object ["BS" .= (T.decodeUtf8 ∘ B64.encode <$> S.toList bs)]
    AVBool b → object ["BOOL" .= b]
    AVList xs → object ["L" .= xs]
    AVMap xs → object ["M" .= xs]
    AVNum n → object ["N" .= T.pack (show n)]
    AVNumSet ns → object ["NS" .= (T.pack ∘ show <$> S.toList ns)]
    AVString s → object ["S" .= s]
    AVNull b → object ["NULL" .= b]
    AVStringSet ss → object ["SS" .= S.toList ss]

instance FromJSON AttributeValue where
  parseJSON =
    withObject "AttributeValue" $ \o → asum
      [ fmap AVBin ∘ parseBin =≪ o .: "B"
      , fmap (AVBinSet ∘ S.fromList) ∘ mapM parseBin =≪ o .: "BS"
      , AVBool <$> o .: "BOOL"
      , AVList <$> o .: "L"
      , AVMap <$> o .: "M"
      , fmap AVNum ∘ parseScientific =≪ o .: "N"
      , fmap (AVNumSet ∘ S.fromList) ∘ mapM parseScientific =≪ o .: "NS"
      , AVString <$> o.: "S"
      , AVStringSet ∘ S.fromList <$> o .: "SS"
      , AVNull <$> o .: "NULL"
      ]

    where
      parseBin =
        either (fail ∘ ("parseBin failed: " ⊕)) pure
          ∘ B64.decode
          ∘ T.encodeUtf8

      parseScientific (String str) =
        either (fail ∘ ("parseScientific failed: " ⊕)) pure $
          Atto.parseOnly Atto.scientific str
      parseScientific (Number n) = pure n
      parseScientific _ = fail "Unexpected JSON type in parseScientific"



-- | A prism for 'AVBin'.
--
-- @
-- '_AVBin' ∷ Prism' 'AttributeValue' 'B.ByteString'
-- @
_AVBin
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p B.ByteString (f B.ByteString)
  → p AttributeValue (f AttributeValue)
_AVBin =
  dimap to fro ∘ right'
    where
      to = \case
        AVBin e → Right e
        e → Left e
      fro = either pure (fmap AVBin)
{-# INLINE _AVBin #-}

-- | A prism for 'AVBool'.
--
-- @
-- '_AVBool' ∷ Prism' 'AttributeValue' 'Bool'
-- @
_AVBool
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p Bool (f Bool)
  → p AttributeValue (f AttributeValue)
_AVBool =
  dimap to fro ∘ right'
    where
      to = \case
        AVBool e → Right e
        e → Left e
      fro = either pure (fmap AVBool)
{-# INLINE _AVBool #-}

-- | A prism for 'AVBinSet'.
--
-- @
-- '_AVBinSet' ∷ Prism' 'AttributeValue' ('S.Set' 'B.ByteString')
-- @
_AVBinSet
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p (S.Set B.ByteString) (f (S.Set B.ByteString))
  → p AttributeValue (f AttributeValue)
_AVBinSet =
  dimap to fro ∘ right'
    where
      to = \case
        AVBinSet e → Right e
        e → Left e
      fro = either pure (fmap AVBinSet)
{-# INLINE _AVBinSet #-}

-- | A prism for 'AVList'.
--
-- @
-- '_AVList' ∷ Prism' 'AttributeValue' ['AttributeValue']
-- @
_AVList
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p [AttributeValue] (f [AttributeValue])
  → p AttributeValue (f AttributeValue)
_AVList =
  dimap to fro ∘ right'
    where
      to = \case
        AVList e → Right e
        e → Left e
      fro = either pure (fmap AVList)
{-# INLINE _AVList #-}

-- | A prism for 'AVMap'.
--
-- @
-- '_AVMap' ∷ Prism' 'AttributeValue' ('M.Map' 'T.Text' 'AttributeValue')
-- @
_AVMap
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p (M.Map T.Text AttributeValue) (f (M.Map T.Text AttributeValue))
  → p AttributeValue (f AttributeValue)
_AVMap =
  dimap to fro ∘ right'
    where
      to = \case
        AVMap e → Right e
        e → Left e
      fro = either pure (fmap AVMap)
{-# INLINE _AVMap #-}

-- | A prism for 'AVNum'.
--
-- @
-- '_AVNum' ∷ Prism' 'AttributeValue' 'Scientific'
-- @
_AVNum
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p Scientific (f Scientific)
  → p AttributeValue (f AttributeValue)
_AVNum =
  dimap to fro ∘ right'
    where
      to = \case
        AVNum e → Right e
        e → Left e
      fro = either pure (fmap AVNum)
{-# INLINE _AVNum #-}

-- | A prism for 'AVNumSet'.
--
-- @
-- '_AVNumSet' ∷ Prism' 'AttributeValue' ('S.Set' 'Scientific')
-- @
_AVNumSet
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p (S.Set Scientific) (f (S.Set Scientific))
  → p AttributeValue (f AttributeValue)
_AVNumSet =
  dimap to fro ∘ right'
    where
      to = \case
        AVNumSet e → Right e
        e → Left e
      fro = either pure (fmap AVNumSet)
{-# INLINE _AVNumSet #-}

-- | A prism for 'AVNull'.
--
-- @
-- '_AVNull' ∷ Prism' 'AttributeValue' 'Bool'
-- @
_AVNull
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p Bool (f Bool)
  → p AttributeValue (f AttributeValue)
_AVNull =
  dimap to fro ∘ right'
    where
      to = \case
        AVNull e → Right e
        e → Left e
      fro = either pure (fmap AVNull)
{-# INLINE _AVNull #-}

-- | A prism for 'AVString'.
--
-- @
-- '_AVString' ∷ Prism' 'AttributeValue' 'T.Text'
-- @
_AVString
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p T.Text (f T.Text)
  → p AttributeValue (f AttributeValue)
_AVString =
  dimap to fro ∘ right'
    where
      to = \case
        AVString e → Right e
        e → Left e
      fro = either pure (fmap AVString)
{-# INLINE _AVString #-}

-- | A prism for 'AVStringSet'.
--
-- @
-- '_AVStringSet' ∷ Prism' 'AttributeValue' ('S.Set' 'T.Text')
-- @
_AVStringSet
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p (S.Set T.Text) (f (S.Set T.Text))
  → p AttributeValue (f AttributeValue)
_AVStringSet =
  dimap to fro ∘ right'
    where
      to = \case
        AVStringSet e → Right e
        e → Left e
      fro = either pure (fmap AVStringSet)
{-# INLINE _AVStringSet #-}

data KeyType
  = KeyTypeHash
  | KeyTypeRange
  deriving (Eq, Ord, Enum, Show, Read, Typeable)

keyTypeToText
  ∷ IsString s
  ⇒ KeyType
  → s
keyTypeToText = \case
  KeyTypeHash → "HASH"
  KeyTypeRange → "RANGE"

instance ToJSON KeyType where
  toJSON = keyTypeToText

instance FromJSON KeyType where
  parseJSON =
    parseEnum "KeyType" keyTypeToText
      [ KeyTypeHash
      , KeyTypeRange
      ]

parseEnum
  ∷ String
  → (α → T.Text)
  → [α]
  → Value
  → Parser α
parseEnum name render opts =
  withText name $ \str →
    asum $ parser str <$> opts

  where
    parser str o =
      o <$ if render o ≡ str
        then pure o
        else empty

-- | A prism for 'KeyTypeHash'.
--
-- @
-- '_KeyTypeHash' ∷ Prism' 'KeyType' ()
-- @
_KeyTypeHash
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p KeyType (f KeyType)
_KeyTypeHash =
  dimap to fro ∘ right'
    where
      to = \case
        KeyTypeHash → Right ()
        e → Left e
      fro = either pure (const $ pure KeyTypeHash)
{-# INLINE _KeyTypeHash #-}

-- | A prism for 'KeyTypeRange'.
--
-- @
-- '_KeyTypeRange' ∷ Prism' 'KeyType' ()
-- @
_KeyTypeRange
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p KeyType (f KeyType)
_KeyTypeRange =
  dimap to fro ∘ right'
    where
      to = \case
        KeyTypeRange → Right ()
        e → Left e
      fro = either pure (const $ pure KeyTypeRange)
{-# INLINE _KeyTypeRange #-}


data KeySchemaElement
  = KeySchemaElement
  { _kseAttributeName ∷ !T.Text
  , _kseKeyType ∷ !KeyType
  } deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON KeySchemaElement where
  toJSON KeySchemaElement{..} = object
    [ "AttributeName" .= _kseAttributeName
    , "KeyType" .= _kseKeyType
    ]

instance FromJSON KeySchemaElement where
  parseJSON =
    withObject "KeySchemaElement" $ \o →
      pure KeySchemaElement
        ⊛ o .: "AttributeName"
        ⊛ o .: "KeyType"

-- | A lens for '_kseAttributeName'.
--
-- @
-- 'kseAttributeName' ∷ Lens' 'KeySchemaElement' 'T.Text'
-- @
--
kseAttributeName
  ∷ Functor f
  ⇒ (T.Text → f T.Text)
  → KeySchemaElement
  → f KeySchemaElement
kseAttributeName i KeySchemaElement{..} =
  (\_kseAttributeName → KeySchemaElement{..})
    <$> i _kseAttributeName
{-# INLINE kseAttributeName #-}

-- | A lens for '_kseKeyType'.
--
-- @
-- 'kseKeyType' ∷ Lens' 'KeySchemaElement' 'KeyType'
-- @
--
kseKeyType
  ∷ Functor f
  ⇒ (KeyType → f KeyType)
  → KeySchemaElement
  → f KeySchemaElement
kseKeyType i KeySchemaElement{..} =
  (\_kseKeyType → KeySchemaElement{..})
    <$> i _kseKeyType
{-# INLINE kseKeyType #-}


data StreamViewType
  = StreamViewKeysOnly
    -- ^ only the key attributes of the modified item
  | StreamViewNewImage
    -- ^ the entire item, as it appears after it was modified
  | StreamViewOldImage
    -- ^ the entire item, as it appeared before it was modified
  | StreamViewNewAndOldImages
    -- ^ both the new and the old images of the item
  deriving (Eq, Ord, Enum, Show, Read, Typeable)

streamViewTypeToText
  ∷ IsString s
  ⇒ StreamViewType
  → s
streamViewTypeToText = \case
  StreamViewKeysOnly → "KEYS_ONLY"
  StreamViewNewImage → "NEW_IMAGE"
  StreamViewOldImage → "OLD_IMAGE"
  StreamViewNewAndOldImages → "NEW_AND_OLD_IMAGES"

instance ToJSON StreamViewType where
  toJSON = streamViewTypeToText

instance FromJSON StreamViewType where
  parseJSON =
    parseEnum "StreamViewType" streamViewTypeToText
      [ StreamViewKeysOnly
      , StreamViewNewImage
      , StreamViewOldImage
      , StreamViewNewAndOldImages
      ]

-- | A prism for 'StreamViewKeysOnly'.
--
-- @
-- '_StreamViewKeysOnly' ∷ Prism' 'StreamViewType' ()
-- @
_StreamViewKeysOnly
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamViewType (f StreamViewType)
_StreamViewKeysOnly =
  dimap to fro ∘ right'
    where
      to = \case
        StreamViewKeysOnly → Right ()
        e → Left e
      fro = either pure (const $ pure StreamViewKeysOnly)
{-# INLINE _StreamViewKeysOnly #-}

-- | A prism for 'StreamViewNewImage'.
--
-- @
-- '_StreamViewNewImage' ∷ Prism' 'StreamViewType' ()
-- @
_StreamViewNewImage
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamViewType (f StreamViewType)
_StreamViewNewImage =
  dimap to fro ∘ right'
    where
      to = \case
        StreamViewNewImage → Right ()
        e → Left e
      fro = either pure (const $ pure StreamViewNewImage)
{-# INLINE _StreamViewNewImage #-}

-- | A prism for 'StreamViewOldImage'.
--
-- @
-- '_StreamViewOldImage' ∷ Prism' 'StreamViewType' ()
-- @
_StreamViewOldImage
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamViewType (f StreamViewType)
_StreamViewOldImage =
  dimap to fro ∘ right'
    where
      to = \case
        StreamViewOldImage → Right ()
        e → Left e
      fro = either pure (const $ pure StreamViewOldImage)
{-# INLINE _StreamViewOldImage #-}

-- | A prism for 'StreamViewNewAndOldImages'.
--
-- @
-- '_StreamViewNewAndOldImages' ∷ Prism' 'StreamViewType' ()
-- @
_StreamViewNewAndOldImages
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamViewType (f StreamViewType)
_StreamViewNewAndOldImages =
  dimap to fro ∘ right'
    where
      to = \case
        StreamViewNewAndOldImages → Right ()
        e → Left e
      fro = either pure (const $ pure StreamViewNewAndOldImages)
{-# INLINE _StreamViewNewAndOldImages #-}

data StreamRecord
  = StreamRecord
  { _strKeys ∷ !(Maybe (M.Map T.Text AttributeValue))
  , _strNewImage ∷ !(Maybe (M.Map T.Text AttributeValue))
  , _strOldImage ∷ !(Maybe (M.Map T.Text AttributeValue))
  , _strSequenceNumber ∷ !(Maybe SequenceNumber)
  , _strSizeBytes ∷ !(Maybe Integer)
  , _strStreamViewType ∷ !(Maybe StreamViewType)
  } deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON StreamRecord where
  toJSON StreamRecord{..} = object
    [ "Keys" .= _strKeys
    , "NewImage" .= _strNewImage
    , "OldImage" .= _strOldImage
    , "SequenceNumber" .= _strSequenceNumber
    , "SizeBytes" .= _strSizeBytes
    , "StreamViewType" .= _strStreamViewType
    ]

instance FromJSON StreamRecord where
  parseJSON =
    withObject "StreamRecord" $ \o →
      pure StreamRecord
        ⊛ o .:? "Keys"
        ⊛ o .:? "NewImage"
        ⊛ o .:? "OldImage"
        ⊛ o .:? "SequenceNumber"
        ⊛ o .:? "SizeBytes"
        ⊛ o .:? "StreamViewType"

-- | A lens for '_strKeys'.
--
-- @
-- 'strKeys' ∷ Lens' 'StreamRecord' ('Maybe' ('M.Map' 'T.Text' 'AttributeValue'))
-- @
--
strKeys
  ∷ Functor f
  ⇒ (Maybe (M.Map T.Text AttributeValue) → f (Maybe (M.Map T.Text AttributeValue)))
  → StreamRecord
  → f StreamRecord
strKeys i StreamRecord{..} =
  (\_strKeys → StreamRecord{..})
    <$> i _strKeys
{-# INLINE strKeys #-}

-- | A lens for '_strNewImage'.
--
-- @
-- 'strNewImage' ∷ Lens' 'StreamRecord' ('Maybe' ('M.Map' 'T.Text' 'AttributeValue'))
-- @
--
strNewImage
  ∷ Functor f
  ⇒ (Maybe (M.Map T.Text AttributeValue) → f (Maybe (M.Map T.Text AttributeValue)))
  → StreamRecord
  → f StreamRecord
strNewImage i StreamRecord{..} =
  (\_strNewImage → StreamRecord{..})
    <$> i _strNewImage
{-# INLINE strNewImage #-}

-- | A lens for '_strOldImage'.
--
-- @
-- 'strOldImage' ∷ Lens' 'StreamRecord' ('Maybe' ('M.Map' 'T.Text' 'AttributeValue'))
-- @
--
strOldImage
  ∷ Functor f
  ⇒ (Maybe (M.Map T.Text AttributeValue) → f (Maybe (M.Map T.Text AttributeValue)))
  → StreamRecord
  → f StreamRecord
strOldImage i StreamRecord{..} =
  (\_strOldImage → StreamRecord{..})
    <$> i _strOldImage
{-# INLINE strOldImage #-}

-- | A lens for '_strSequenceNumber'.
--
-- @
-- 'strSequenceNumber' ∷ Lens' 'StreamRecord' ('Maybe' 'SequenceNumber')
-- @
--
strSequenceNumber
  ∷ Functor f
  ⇒ (Maybe SequenceNumber → f (Maybe SequenceNumber))
  → StreamRecord
  → f StreamRecord
strSequenceNumber i StreamRecord{..} =
  (\_strSequenceNumber → StreamRecord{..})
    <$> i _strSequenceNumber
{-# INLINE strSequenceNumber #-}

-- | A lens for '_strSizeBytes'.
--
-- @
-- 'strSizeBytes' ∷ Lens' 'StreamRecord' ('Maybe' 'Integer')
-- @
--
strSizeBytes
  ∷ Functor f
  ⇒ (Maybe Integer → f (Maybe Integer))
  → StreamRecord
  → f StreamRecord
strSizeBytes i StreamRecord{..} =
  (\_strSizeBytes → StreamRecord{..})
    <$> i _strSizeBytes
{-# INLINE strSizeBytes #-}

-- | A lens for '_strStreamViewType'.
--
-- @
-- 'strStreamViewType' ∷ Lens' 'StreamRecord' ('Maybe' 'StreamViewType')
-- @
--
strStreamViewType
  ∷ Functor f
  ⇒ (Maybe StreamViewType → f (Maybe StreamViewType))
  → StreamRecord
  → f StreamRecord
strStreamViewType i StreamRecord{..} =
  (\_strStreamViewType → StreamRecord{..})
    <$> i _strStreamViewType
{-# INLINE strStreamViewType #-}


-- | Identifier for a stream.
--
-- Length constraints: @56 ≤ n ≤ 128@.
--
newtype StreamId
  = StreamId
  { _stidText ∷ T.Text
  } deriving (Eq, Ord, Typeable, Show, Read)

instance ToJSON StreamId where
  toJSON = toJSON ∘ _stidText

instance FromJSON StreamId where
  parseJSON =
    withText "StreamId" $
      pure ∘ StreamId

data StreamStatus
  = StatusEnabling
    -- ^ DynamoDB Streams is currently being enabled on the table
  | StatusEnabled
    -- ^ The stream is enabled
  | StatusDisabling
    -- ^ DynamoDB Streams is currently being disabled on the table
  | StatusDisabled
    -- ^ The stream is disabled
  deriving (Eq, Ord, Enum, Show, Read, Typeable)

streamStatusToText
  ∷ IsString a
  ⇒ StreamStatus
  → a
streamStatusToText = \case
  StatusEnabling → "ENABLING"
  StatusEnabled → "ENABLED"
  StatusDisabling → "DISABLING"
  StatusDisabled → "DISABLED"

instance ToJSON StreamStatus where
  toJSON = streamStatusToText

instance FromJSON StreamStatus where
  parseJSON =
    parseEnum "StreamStatus" streamStatusToText
      [ StatusEnabling
      , StatusEnabled
      , StatusDisabling
      , StatusDisabled
      ]

-- | A prism for 'StatusEnabling'.
--
-- @
-- '_StatusEnabling' ∷ Prism' 'StreamStatus' ()
-- @
_StatusEnabling
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamStatus (f StreamStatus)
_StatusEnabling =
  dimap to fro ∘ right'
    where
      to = \case
        StatusEnabling → Right ()
        e → Left e
      fro = either pure (const $ pure StatusEnabling)
{-# INLINE _StatusEnabling #-}

-- | A prism for 'StatusEnabled'.
--
-- @
-- '_StatusEnabled' ∷ Prism' 'StreamStatus' ()
-- @
_StatusEnabled
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamStatus (f StreamStatus)
_StatusEnabled =
  dimap to fro ∘ right'
    where
      to = \case
        StatusEnabled → Right ()
        e → Left e
      fro = either pure (const $ pure StatusEnabled)
{-# INLINE _StatusEnabled #-}

-- | A prism for 'StatusDisabling'.
--
-- @
-- '_StatusDisabling' ∷ Prism' 'StreamStatus' ()
-- @
_StatusDisabling
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamStatus (f StreamStatus)
_StatusDisabling =
  dimap to fro ∘ right'
    where
      to = \case
        StatusDisabling → Right ()
        e → Left e
      fro = either pure (const $ pure StatusDisabling)
{-# INLINE _StatusDisabling #-}

-- | A prism for 'StatusDisabled'.
--
-- @
-- '_StatusDisabled' ∷ Prism' 'StreamStatus' ()
-- @
_StatusDisabled
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p StreamStatus (f StreamStatus)
_StatusDisabled =
  dimap to fro ∘ right'
    where
      to = \case
        StatusDisabled → Right ()
        e → Left e
      fro = either pure (const $ pure StatusDisabled)
{-# INLINE _StatusDisabled #-}


data StreamDescription
  = StreamDescription
  { _sdCreationRequestDateTime ∷ !(Maybe UTCTime)
  , _sdKeySchema ∷ ![KeySchemaElement]
  , _sdLastEvaluatedShardId ∷ !(Maybe ShardId)
  , _sdShards ∷ ![Shard]
  , _sdStreamARN ∷ !(Maybe T.Text)
  , _sdStreamId ∷ !(Maybe StreamId)
  , _sdStreamStatus ∷ !(Maybe StreamStatus)
  , _sdStreamViewType ∷ !(Maybe StreamViewType)
  , _sdTableName ∷ !(Maybe T.Text)
  } deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON StreamDescription where
  toJSON StreamDescription{..} = object
    [ "CreationRequestDateTime" .= fmap formatDateTime' _sdCreationRequestDateTime
    , "KeySchema" .= _sdKeySchema
    , "Shards" .= _sdShards
    , "StreamARN" .= _sdStreamARN
    , "StreamId" .= _sdStreamId
    , "StreamStatus" .= _sdStreamStatus
    , "StreamViewType" .= _sdStreamViewType
    , "TableName" .= _sdTableName
    ]
    where
      formatDateTime' =
        formatTime defaultTimeLocale iso8601UtcDate

instance FromJSON StreamDescription where
  parseJSON =
    withObject "StreamDescription" $ \o →
      pure StreamDescription
        ⊛ (parseDateTime' =<< o .:? "CreationRequestDateTime")
        ⊛ o .:? "KeySchema" .!= []
        ⊛ o .:? "LastEvaluatedShardId"
        ⊛ o .:? "Shards" .!= []
        ⊛ o .:? "StreamARN"
        ⊛ o .:? "StreamId"
        ⊛ o .:? "StreamStatus"
        ⊛ o .:? "StreamViewType"
        ⊛ o .:? "TableName"
    where
      parseDateTime' =
        maybe empty pure
          ∘ fmap parseHttpDate

-- | A lens for '_sdCreationRequestDateTime'.
--
-- @
-- 'sdCreationRequestDateTime' ∷ Lens' 'StreamDescription' ('Maybe' 'UTCTime')
-- @
--
sdCreationRequestDateTime
  ∷ Functor f
  ⇒ (Maybe UTCTime → f (Maybe UTCTime))
  → StreamDescription
  → f StreamDescription
sdCreationRequestDateTime i StreamDescription{..} =
  (\_sdCreationRequestDateTime → StreamDescription{..})
    <$> i _sdCreationRequestDateTime
{-# INLINE sdCreationRequestDateTime #-}

-- | A lens for '_sdKeySchema'.
--
-- @
-- 'sdKeySchema' ∷ Lens' 'StreamDescription' ['KeySchemaElement']
-- @
--
sdKeySchema
  ∷ Functor f
  ⇒ ([KeySchemaElement] → f [KeySchemaElement])
  → StreamDescription
  → f StreamDescription
sdKeySchema i StreamDescription{..} =
  (\_sdKeySchema → StreamDescription{..})
    <$> i _sdKeySchema
{-# INLINE sdKeySchema #-}

-- | A lens for '_sdLastEvaluatedShardId'.
--
-- @
-- 'sdLastEvaluatedShardId' ∷ Lens' 'StreamDescription' ('Maybe' 'ShardId')
-- @
--
sdLastEvaluatedShardId
  ∷ Functor f
  ⇒ (Maybe ShardId → f (Maybe ShardId))
  → StreamDescription
  → f StreamDescription
sdLastEvaluatedShardId i StreamDescription{..} =
  (\_sdLastEvaluatedShardId → StreamDescription{..})
    <$> i _sdLastEvaluatedShardId
{-# INLINE sdLastEvaluatedShardId #-}

-- | A lens for '_sdShards'.
--
-- @
-- 'sdShards' ∷ Lens' 'StreamDescription' ['Shard']
-- @
--
sdShards
  ∷ Functor f
  ⇒ ([Shard] → f [Shard])
  → StreamDescription
  → f StreamDescription
sdShards i StreamDescription{..} =
  (\_sdShards → StreamDescription{..})
    <$> i _sdShards
{-# INLINE sdShards #-}

-- | A lens for '_sdStreamARN'.
--
-- @
-- 'sdStreamARN' ∷ Lens' 'StreamDescription' ('Maybe' 'T.Text')
-- @
--
sdStreamARN
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → StreamDescription
  → f StreamDescription
sdStreamARN i StreamDescription{..} =
  (\_sdStreamARN → StreamDescription{..})
    <$> i _sdStreamARN
{-# INLINE sdStreamARN #-}

-- | A lens for '_sdStreamId'.
--
-- @
-- 'sdStreamId' ∷ Lens' 'StreamDescription' ('Maybe' 'StreamId')
-- @
--
sdStreamId
  ∷ Functor f
  ⇒ (Maybe StreamId → f (Maybe StreamId))
  → StreamDescription
  → f StreamDescription
sdStreamId i StreamDescription{..} =
  (\_sdStreamId → StreamDescription{..})
    <$> i _sdStreamId
{-# INLINE sdStreamId #-}

-- | A lens for '_sdStreamStatus'.
--
-- @
-- 'sdStreamStatus' ∷ Lens' 'StreamDescription' ('Maybe' 'StreamStatus')
-- @
--
sdStreamStatus
  ∷ Functor f
  ⇒ (Maybe StreamStatus → f (Maybe StreamStatus))
  → StreamDescription
  → f StreamDescription
sdStreamStatus i StreamDescription{..} =
  (\_sdStreamStatus → StreamDescription{..})
    <$> i _sdStreamStatus
{-# INLINE sdStreamStatus #-}

-- | A lens for '_sdStreamViewType'.
--
-- @
-- 'sdStreamViewType' ∷ Lens' 'StreamDescription' ('Maybe' 'StreamViewType')
-- @
--
sdStreamViewType
  ∷ Functor f
  ⇒ (Maybe StreamViewType → f (Maybe StreamViewType))
  → StreamDescription
  → f StreamDescription
sdStreamViewType i StreamDescription{..} =
  (\_sdStreamViewType → StreamDescription{..})
    <$> i _sdStreamViewType
{-# INLINE sdStreamViewType #-}

-- | A lens for '_sdTableName'.
--
-- @
-- 'sdTableName' ∷ Lens' 'StreamDescription' ('Maybe' 'T.Text')
-- @
--
sdTableName
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → StreamDescription
  → f StreamDescription
sdTableName i StreamDescription{..} =
  (\_sdTableName → StreamDescription{..})
    <$> i _sdTableName
{-# INLINE sdTableName #-}


-- | A globally unique identifier for the event that was recorded.
--
newtype EventId
  = EventId
  { _eidText ∷ T.Text
  } deriving (Eq, Ord, Typeable, Show, Read)

instance ToJSON EventId where
  toJSON = toJSON ∘ _eidText

instance FromJSON EventId where
  parseJSON =
    withText "EventId" $
      pure ∘ EventId

data EventName
  = EventInsert
  | EventModify
  | EventRemove
  deriving (Eq, Ord, Enum, Typeable, Show, Read)

eventNameToText
  ∷ IsString s
  ⇒ EventName
  → s
eventNameToText = \case
  EventInsert → "INSERT"
  EventModify → "MODIFY"
  EventRemove → "REMOVE"

instance ToJSON EventName where
  toJSON = eventNameToText

instance FromJSON EventName where
  parseJSON =
    parseEnum "EventName" eventNameToText
      [ EventInsert
      , EventModify
      , EventRemove
      ]

-- | A prism for 'EventInsert'.
--
-- @
-- '_EventInsert' ∷ Prism' 'EventName' ()
-- @
_EventInsert
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p EventName (f EventName)
_EventInsert =
  dimap to fro ∘ right'
    where
      to = \case
        EventInsert → Right ()
        e → Left e
      fro = either pure (const $ pure EventInsert)
{-# INLINE _EventInsert #-}

-- | A prism for 'EventModify'.
--
-- @
-- '_EventModify' ∷ Prism' 'EventName' ()
-- @
_EventModify
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p EventName (f EventName)
_EventModify =
  dimap to fro ∘ right'
    where
      to = \case
        EventModify → Right ()
        e → Left e
      fro = either pure (const $ pure EventModify)
{-# INLINE _EventModify #-}

-- | A prism for 'EventRemove'.
--
-- @
-- '_EventRemove' ∷ Prism' 'EventName' ()
-- @
_EventRemove
  ∷ ( Choice p
    , Applicative f
    )
  ⇒ p () (f ())
  → p EventName (f EventName)
_EventRemove =
  dimap to fro ∘ right'
    where
      to = \case
        EventRemove → Right ()
        e → Left e
      fro = either pure (const $ pure EventRemove)
{-# INLINE _EventRemove #-}


data Record
  = Record
  { _rAwsRegion ∷ !(Maybe Region)
  , _rStreamRecord ∷ !(Maybe StreamRecord)
  , _rEventId ∷ !(Maybe EventId)
  , _rEventName ∷ !(Maybe EventName)
  , _rEventSource ∷ !(Maybe T.Text)
  , _rEventVersion ∷ !(Maybe T.Text)
  } deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON Record where
  toJSON Record{..} = object
    [ "awsRegion" .= (String ∘ regionToText <$> _rAwsRegion)
    , "dynamodb" .= _rStreamRecord
    , "eventID" .= _rEventId
    , "eventName" .= _rEventName
    , "eventSource" .= _rEventSource
    , "eventVersion" .= _rEventVersion
    ]

instance FromJSON Record where
  parseJSON =
    withObject "Record" $ \o →
      pure Record
        ⊛ (traverse (either fail pure ∘ fromText) =≪ o .:? "awsRegion")
        ⊛ o .:? "dynamodb"
        ⊛ o .:? "eventID"
        ⊛ o .:? "eventName"
        ⊛ o .:? "eventSource"
        ⊛ o .:? "eventVersion"

-- | A lens for '_rAwsRegion'.
--
-- @
-- 'rAwsRegion' ∷ Lens' 'Record' ('Maybe' 'Region')
-- @
--
rAwsRegion
  ∷ Functor f
  ⇒ (Maybe Region → f (Maybe Region))
  → Record
  → f Record
rAwsRegion i Record{..} =
  (\_rAwsRegion → Record{..})
    <$> i _rAwsRegion
{-# INLINE rAwsRegion #-}

-- | A lens for '_rStreamRecord'.
--
-- @
-- 'rStreamRecord' ∷ Lens' 'Record' ('Maybe' 'StreamRecord')
-- @
--
rStreamRecord
  ∷ Functor f
  ⇒ (Maybe StreamRecord → f (Maybe StreamRecord))
  → Record
  → f Record
rStreamRecord i Record{..} =
  (\_rStreamRecord → Record{..})
    <$> i _rStreamRecord
{-# INLINE rStreamRecord #-}

-- | A lens for '_rEventId'.
--
-- @
-- 'rEventId' ∷ Lens' 'Record' ('Maybe' 'EventId')
-- @
--
rEventId
  ∷ Functor f
  ⇒ (Maybe EventId → f (Maybe EventId))
  → Record
  → f Record
rEventId i Record{..} =
  (\_rEventId → Record{..})
    <$> i _rEventId
{-# INLINE rEventId #-}

-- | A lens for '_rEventName'.
--
-- @
-- 'rEventName' ∷ Lens' 'Record' ('Maybe' 'EventName')
-- @
--
rEventName
  ∷ Functor f
  ⇒ (Maybe EventName → f (Maybe EventName))
  → Record
  → f Record
rEventName i Record{..} =
  (\_rEventName → Record{..})
    <$> i _rEventName
{-# INLINE rEventName #-}

-- | A lens for '_rEventSource'.
--
-- @
-- 'rEventSource' ∷ Lens' 'Record' ('Maybe' 'T.Text')
-- @
--
rEventSource
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → Record
  → f Record
rEventSource i Record{..} =
  (\_rEventSource → Record{..})
    <$> i _rEventSource
{-# INLINE rEventSource #-}

-- | A lens for '_rEventVersion'.
--
-- @
-- 'rEventVersion' ∷ Lens' 'Record' ('Maybe' 'T.Text')
-- @
--
rEventVersion
  ∷ Functor f
  ⇒ (Maybe T.Text → f (Maybe T.Text))
  → Record
  → f Record
rEventVersion i Record{..} =
  (\_rEventVersion → Record{..})
    <$> i _rEventVersion
{-# INLINE rEventVersion #-}


