{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types ( Epoch
             , BlockHeight
             , Slot
             , LightState(..)
             , CommitteeIndex
             , DomainType(..)
             , ValidatorIndex
             , Validator(..)
             , ValidatorsData(..)
             , CommitteeData(..)
             , CommitteeDataEntry(..)
             , LightBlock(..)
             ) where

import Data.Word ( Word64 )
-- import Data.LargeWord
import Data.ByteString as BS ( ByteString, reverse )
import GHC.Generics
import Data.Aeson
import Serialize ( serializeInteger )
import Data.Vector ( Vector )
import qualified Data.Vector as V

-- NOTE: as a first pass, I'm making everything the same type (Integer and ByteString mostly) as I don't have to deal with
-- type conversion. I'm making manual range checks with asserts
-- This is easier to at least try the implementation.
-- I'll switch to proper types with constructors, etc. later.

-- | Epoch number are positive and always increasing
type Epoch = Word64

-- | The block height is the block number, positive and always increasing
type BlockHeight = Word64

-- | Slot number are positive and always increasing. A slot number gives the epoch in which it lived
type Slot = Word64

-- type Word48 = LargeKey Word16 Word32

-- | A "validator registry index"
-- This is the validator's position in the State's list of all validators. This is used in the original impl
-- because it's lighter to carry around than the full Validator type (do we need this in Haskell thanks to lazyness?)
type ValidatorIndex = Word64

-- | A committee index at a slot (where there are several committees for a slot)
type CommitteeIndex = Word64

-- data BLSSignature
-- type BLSSignature = Word96
type BLSSignature = Integer

-- data BLSPubKey
-- type BLSPubKey = Word48
type BLSPubKey = Integer

-- data BLSPrivKey

-- Bytestring?
-- data Message
type Message = ByteString

-- | Represents part of the Beacon Chain State, only what we actually need
data LightState = LightState
    { currSlot         :: Slot
    , validators :: Vector Validator
    , randaoMixes      :: Vector ByteString -- epochsPerHistoricalVector elements
    }

data Validator = Validator
    { pubKey           :: BLSPubKey
    , activationEpoch  :: Epoch
    , exitEpoch        :: Epoch
    , effectiveBalance :: Word64 -- Needed for computing proposer index
    } deriving (Eq, Show, Generic)

instance FromJSON Validator where
    parseJSON = withObject "Validator" $ \v -> do
        o <- v .: "validator"
        Validator
            <$> fmap read (o .: "pubkey")
            <*> fmap read (o .: "activation_epoch")
            <*> fmap read (o .: "exit_epoch")
            <*> fmap read (o .: "effective_balance")

-- | JSON instance data to parse QuickNode's Validator data
-- That's what returned by eth-v1-beacon-states-{state_id}-validators enpoint
data ValidatorsData = ValidatorsData
    { vdExecOptimistic :: Bool
    , vdData           :: Vector Validator
    } deriving (Show, Eq, Generic)

instance FromJSON ValidatorsData where
    parseJSON = withObject "ValidatorsData" $ \o -> ValidatorsData
        <$> o .: "execution_optimistic"
        <*> o .: "data"

data DomainType = DOMAIN_BEACON_PROPOSER
                | DOMAIN_BEACON_ATTESTER
                | DOMAIN_RANDAO
                | DOMAIN_DEPOSIT
                | DOMAIN_VOLUNTARY_EXIT
                | DOMAIN_SELECTION_PROOF
                | DOMAIN_AGGREGATE_AND_PROOF
                | DOMAIN_APPLICATION_MASK
    deriving (Show, Eq)

-- | Verify if a given BLS Signature matches a given message and public key
-- TODO: move out of 'Types.hs', it doesn't belong here
-- verify :: Message -> BLSPubKey -> BLSSignature -> Bool
-- verify = undefined

-- | JSON instance data to parse QuickNode's Committee data
-- That's what is returned by eth-v1-beacon-states-{state_id}-committees endpoint
data CommitteeData = CommitteeData
    { execOptimistic :: Bool
    , cmData         :: {-# UNPACK #-} ![CommitteeDataEntry]
    } deriving (Generic, Show)

instance FromJSON CommitteeData where
    parseJSON = withObject "CommitteeData" $ \v -> CommitteeData
        <$> v .: "execution_optimistic"
        <*> v .: "data"


data CommitteeDataEntry = CommitteeDataEntry
    { cdeIndex :: {-# UNPACK #-} !CommitteeIndex
    , cdeSlot  :: {-# UNPACK #-} !Slot
    , cdeValidators :: {-# UNPACK #-} !(Vector ValidatorIndex)
    } deriving (Generic, Show)

instance FromJSON CommitteeDataEntry where
    parseJSON = withObject "CommitteeDataEntry" $ \v -> CommitteeDataEntry
        <$> fmap read (v .: "index")
        <*> fmap read (v .: "slot")
        -- <*> fmap (map read) (v .: "validators")
        <*> fmap V.fromList (fmap (map read) (v .: "validators"))
        -- <*> fmap (V.fromList . read) (v .: "validators")

-- | Stripped-down version of a Block on the ethereum chain, including only what's needed in here
data LightBlock = LightBlock
    { bNumber :: Word64
    , prevRandao :: ByteString
    , proposerIndex :: Word64
    } deriving (Generic, Show)

instance FromJSON LightBlock where
    parseJSON = withObject "LightBlock" $ \v -> do
        dat     <- v    .: "data"
        msg     <- dat  .: "message"
        body    <- msg  .: "body"
        payload <- body .: "execution_payload"
        LightBlock
            <$> fmap read (payload .: "block_number")
            <*> (BS.reverse <$> fmap (serializeInteger 32) (read <$> (payload .: "prev_randao")))
            -- <*> fmap (reverse (serializeInteger (read <$> (payload .: "prev_randao")) 32))
            <*> fmap read (msg .: "proposer_index")