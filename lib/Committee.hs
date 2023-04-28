module Committee ( getBeaconCommittee, getBeaconProposerIndex ) where

import Types
import Config
import Utils
import Data.ByteString ( ByteString, append )
import Data.Ix ( range )
import Crypto.Hash.SHA256 ( hash )

-- | Return the beacon committee at @slot@ for @index@
getBeaconCommittee :: LightState -> Slot -> CommitteeIndex -> [ValidatorIndex]
getBeaconCommittee state slot index =
    let epoch = epochFromSlot slot
        committeesPerSlot = getCommitteeCountPerSlot state epoch
        indices = getActiveValidatorIndices state epoch
        seed = getSeed state epoch DOMAIN_BEACON_ATTESTER
        index_ = (slot `mod` slotsPerEpoch) * committeesPerSlot + index
        count = committeesPerSlot * slotsPerEpoch
    in computeCommittee indices seed index_ count

-- | Return the committee corresponding to indices, seed, index, and committee count
computeCommittee :: [ValidatorIndex] -> ByteString -> Integer -> Integer -> [ValidatorIndex]
computeCommittee indices seed index count =
    let len = length indices
        start = (toInteger len * index) `div` count
        end   = (toInteger len * (index + 1)) `div` count
    -- in [ indices !! fromInteger ((computeShuffledIndex i (length indices) seed)) | i <- range (start, end) ]
    in [ indices !! fromInteger ((computeShuffledIndex i len seed)) | i <- range (start, end - 1) ] -- Careful of the upper bound not included!

-- | Returns the proposer index at the current slot
-- Not necessarily needed for the bridge, but this allows to check algorithms is okay
getBeaconProposerIndex :: LightState -> ValidatorIndex
getBeaconProposerIndex state =
    let slot = currSlot state
        epoch = epochFromSlot slot
        seed = hash $ (getSeed state epoch DOMAIN_BEACON_PROPOSER) `append` (serializeInteger slot 8)
        indices = getActiveValidatorIndices state epoch
    in computeProposerIndex state indices seed
