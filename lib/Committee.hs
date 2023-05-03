{-# LANGUAGE BangPatterns      #-}

module Committee ( getBeaconCommittee
                --  , getBeaconProposerIndex
                 ) where

import Types
import Config
import Utils
import Data.ByteString ( ByteString, append )
import Data.Ix ( range )
import Crypto.Hash.SHA256 ( hash )
import Serialize
import Debug.Trace ( trace )
import Data.Vector ( Vector )
import qualified Data.Vector as V

-- | Return the beacon committee at @slot@ for @index@
-- ORIGINAL VERSION
-- getBeaconCommittee :: LightState -> Slot -> CommitteeIndex -> [ValidatorIndex]
-- getBeaconCommittee state slot index =
--     let epoch = epochFromSlot slot
--         committeesPerSlot = getCommitteeCountPerSlot state epoch
--         indices = getActiveValidatorIndices state epoch
--         seed = getSeed state epoch DOMAIN_BEACON_ATTESTER
--         index_ = (slot `mod` slotsPerEpoch) * committeesPerSlot + index
--         count = committeesPerSlot * slotsPerEpoch
--     in computeCommittee indices seed index_ count

-- | Return the beacon committee at @slot@ for @index@
-- We pass the list of previously-computed active validators instead of re-computing it
-- OPTIMIZED VERSION
-- getBeaconCommittee :: LightState -> [ValidatorIndex] -> Integer -> Slot -> CommitteeIndex -> [ValidatorIndex]
getBeaconCommittee :: LightState -> Vector ValidatorIndex -> Integer -> Slot -> CommitteeIndex -> Vector ValidatorIndex
getBeaconCommittee state !activeIndices !len slot index =
    let epoch = epochFromSlot slot
        committeesPerSlot = getCommitteeCountPerSlot state epoch
        -- indices = getActiveValidatorIndices state epoch
        seed = getSeed state epoch DOMAIN_BEACON_ATTESTER
        index_ = (slot `mod` slotsPerEpoch) * committeesPerSlot + index
        count = committeesPerSlot * slotsPerEpoch
    in computeCommittee activeIndices seed index_ count len

-- | Return the committee corresponding to indices, seed, index, and committee count
-- ORIGINAL version
-- computeCommittee :: [ValidatorIndex] -> ByteString -> Integer -> Integer -> [ValidatorIndex]
-- computeCommittee indices seed index count =
--     let len = trace ("\t\tComputing committee #" ++ (show index)) (length indices)
--         start = (toInteger len * index) `div` count
--         end   = (toInteger len * (index + 1)) `div` count
--     in [ (fromInteger i) | i <- range (start, end - 1) ]
--     -- in [ indices !! (fromInteger i) | i <- range (start, end - 1) ] -- Careful of the upper bound not included!
--     -- in [ indices !! fromInteger ((computeShuffledIndex i len seed)) | i <- range (start, end - 1) ] -- Careful of the upper bound not included!

-- | Return the committee corresponding to indices, seed, index, and committee count
-- OPTIMIZED version 
computeCommittee :: Vector ValidatorIndex -> ByteString -> Integer -> Integer -> Integer -> Vector ValidatorIndex
computeCommittee indices seed index count len =
    -- let len = trace ("\t\tComputing committee #" ++ (show index)) (length indices)
    let start = trace ("\t\tComputing committee #" ++ (show index)) $ (len * index) `div` count
        end   = (len * (index + 1)) `div` count
    -- in V.fromList [ (fromInteger i) | i <- range (start, end - 1) ]
    -- in V.fromList [ indices V.! (fromInteger i) | i <- range (start, end - 1) ]  -- Careful of the upper bound not included!
    in V.fromList [ indices V.! fromInteger ((computeShuffledIndex i len seed)) | i <- range (start, end - 1) ] -- Careful of the upper bound not included!

-- | Returns the proposer index at the current slot
-- Not necessarily needed for the bridge, but this allows to check algorithms is okay
-- getBeaconProposerIndex :: LightState -> ValidatorIndex
-- getBeaconProposerIndex state =
--     let slot = currSlot state
--         epoch = epochFromSlot slot
--         seed = hash $ (getSeed state epoch DOMAIN_BEACON_PROPOSER) `append` (serializeInteger 8 slot)
--         indices = getActiveValidatorIndices state epoch
--     in computeProposerIndex state indices seed
