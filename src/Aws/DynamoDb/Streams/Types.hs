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
, shShardId
, shParentShardId
, shSequenceNumberRange
  -- * Attribute values
, AttributeValue(..)
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
, KeyType(..)
, keyTypeToText
, _KeyTypeHash
, _KeyTypeRange
, KeySchemaElement(..)
, kseAttributeName
, kseKeyType
  -- * Stream Records
, StreamViewType(..)
, _StreamViewKeysOnly
, _StreamViewNewImage
, _StreamViewOldImage
, _StreamViewNewAndOldImages
, StreamRecord(..)
, strKeys
, strNewImage
, strOldImage
, strSequenceNumber
, strSizeBytes
, strStreamViewType
) where

import Control.Applicative
import Control.Applicative.Unicode
import Control.Monad.Unicode
import Data.Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Monoid.Unicode
import Data.Profunctor
import Data.Scientific
import Data.String
import Data.Typeable
import Prelude.Unicode

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
    withObject "AttributeValue" $ \o → do
      foldr (<|>) empty $
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
    withText "KeyType" $ \str →
      foldr (<|>) empty ∘ fmap (parser str) $
        [ KeyTypeHash
        , KeyTypeRange
        ]

    where
      parser str kt =
        kt <$
          if (keyTypeToText kt ≡ str)
            then pure kt
            else empty

-- | A prism for 'KeyTypeHash'.
--
-- @
-- '_KeyTypeHash' ∷ Prism' 'KeyType' '()'
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
-- '_KeyTypeRange' ∷ Prism' 'KeyType' '()'
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
    withText "StreamViewType" $ \str →
      foldr (<|>) empty ∘ fmap (parser str) $
        [ StreamViewKeysOnly
        , StreamViewNewImage
        , StreamViewOldImage
        , StreamViewNewAndOldImages
        ]

    where
      parser str vt =
        vt <$
          if (streamViewTypeToText vt ≡ str)
            then pure vt
            else empty

-- | A prism for 'StreamViewKeysOnly'.
--
-- @
-- '_StreamViewKeysOnly' ∷ Prism' 'StreamViewType' '()'
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
-- '_StreamViewNewImage' ∷ Prism' 'StreamViewType' '()'
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
-- '_StreamViewOldImage' ∷ Prism' 'StreamViewType' '()'
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
-- '_StreamViewNewAndOldImages' ∷ Prism' 'StreamViewType' '()'
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
        ⊛ o .: "Keys"
        ⊛ o .: "NewImage"
        ⊛ o .: "OldImage"
        ⊛ o .: "SequenceNumber"
        ⊛ o .: "SizeBytes"
        ⊛ o .: "StreamViewType"

-- | A lens for '_strKeys'.
--
-- @
-- 'strKeys' ∷ Lens' 'StreamRecord' ('Maybe' ('M.Map' 'T.Text' 'AttributeValue'))
-- @
--
strKeys
  ∷ Functor f
  ⇒ ((Maybe (M.Map T.Text AttributeValue)) → f (Maybe (M.Map T.Text AttributeValue)))
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
  ⇒ ((Maybe (M.Map T.Text AttributeValue)) → f (Maybe (M.Map T.Text AttributeValue)))
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
  ⇒ ((Maybe (M.Map T.Text AttributeValue)) → f (Maybe (M.Map T.Text AttributeValue)))
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
  ⇒ ((Maybe SequenceNumber) → f (Maybe SequenceNumber))
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
  ⇒ ((Maybe Integer) → f (Maybe Integer))
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
  ⇒ ((Maybe StreamViewType) → f (Maybe StreamViewType))
  → StreamRecord
  → f StreamRecord
strStreamViewType i StreamRecord{..} =
  (\_strStreamViewType → StreamRecord{..})
    <$> i _strStreamViewType
{-# INLINE strStreamViewType #-}


