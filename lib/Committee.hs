{-# LANGUAGE BangPatterns      #-}

module Committee ( getBeaconCommittee
                --  , getBeaconProposerIndex
                 ) where

import Types
import Config ( shuffleRoundCount, slotsPerEpoch )
import Utils
import Data.ByteString ( ByteString )
import Debug.Trace ( trace )
import Data.Word ( Word64 )
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (unless)

{-

I think I'm doing this wrong: I _think_ (but still unclear) that we need to shuffle ONCE (per what? seed? epoch?)
and then take the sublists/slices. I feel like we are re-computing the same shuffle way too many times
=> This seems to be "per epoch" as hinted here: https://github.com/sigp/lighthouse/blob/a53830fd60a119bf3f659b253360af8027128e83/consensus/types/src/beacon_state/committee_cache.rs#L20

-}

-- | Returns the beacon committee at @slot@ for @index@
--   We pass the list of previously-computed active validators instead of re-computing it
getBeaconCommittee :: MV.IOVector Int -> LightState -> MV.IOVector ValidatorIndex -> Word64 -> Slot -> CommitteeIndex -> IO (MV.IOVector ValidatorIndex)
getBeaconCommittee isShuffled state !activeIndices !len slot indexInSlot = do
    let epoch = epochFromSlot slot
        seed = getSeed state epoch DOMAIN_BEACON_ATTESTER
    committeesPerSlot <- getCommitteeCountPerSlot state epoch
    let indexInEpoch = (slot `mod` slotsPerEpoch) * committeesPerSlot + indexInSlot
        count = committeesPerSlot * slotsPerEpoch
    -- activeIndices <- MV.clone activeIndices_
    -- Shuffle the list once
    -- shuffleList
    computeCommittee isShuffled activeIndices seed indexInEpoch count len

-- | Returns the committee corresponding to indices, seed, index, and committee count
computeCommittee :: MV.IOVector Int -> MV.IOVector ValidatorIndex -> ByteString -> Word64 -> Word64 -> Word64 -> IO (MV.IOVector ValidatorIndex)
computeCommittee isShuffled indices seed indexInEpoch count len = do
    -- let len = trace ("\t\tComputing committee #" ++ (show index)) (length indices)
    -- let start = trace ("\t\tComputing committee #" ++ (show indexInEpoch)) $ fromIntegral $ (len * indexInEpoch) `div` count
    let start = fromIntegral $ (len * indexInEpoch) `div` count
        end   = fromIntegral $ (len * (indexInEpoch + 1)) `div` count

    -- Following of the dirty trick (just s a test, will disappear in Prod)
    isShuffled' <- MV.read isShuffled 0
    unless (isShuffled' == 0) $ do
        shuffleList indices seed (fromIntegral shuffleRoundCount - 1)
        -- MV.set isShuffled True
        MV.modify isShuffled (\v -> v-1) 0
    return $ MV.slice start (end - start) indices