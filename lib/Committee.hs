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

-- | Returns the beacon committee at @slot@ for @index@
--   We pass the list of previously-computed active validators instead of re-computing it
getBeaconCommittee :: LightState -> MV.IOVector ValidatorIndex -> Word64 -> Slot -> CommitteeIndex -> IO (MV.IOVector ValidatorIndex)
getBeaconCommittee state !activeIndices_ !len slot index = do
    let epoch = epochFromSlot slot
        seed = getSeed state epoch DOMAIN_BEACON_ATTESTER
    committeesPerSlot <- getCommitteeCountPerSlot state epoch
    let index_ = (slot `mod` slotsPerEpoch) * committeesPerSlot + index
        count = committeesPerSlot * slotsPerEpoch
    activeIndices <- MV.clone activeIndices_
    computeCommittee activeIndices seed index_ count len

-- | Returns the committee corresponding to indices, seed, index, and committee count
computeCommittee :: MV.IOVector ValidatorIndex -> ByteString -> Word64 -> Word64 -> Word64 -> IO (MV.IOVector ValidatorIndex)
computeCommittee indices seed index count len = do
    -- let len = trace ("\t\tComputing committee #" ++ (show index)) (length indices)
    let start = trace ("\t\tComputing committee #" ++ (show index)) $ fromIntegral $ (len * index) `div` count
        end   = fromIntegral $ (len * (index + 1)) `div` count
    shuffleList indices seed (fromIntegral shuffleRoundCount - 1)
    return $ MV.slice start (end - 1 - start + 1) indices
    -- in V.fromList [ (fromInteger i) | i <- range (start, end - 1) ]
    -- in V.fromList [ indices V.! (fromInteger i) | i <- range (start, end - 1) ]  -- Careful of the upper bound not included!
    -- in U.fromList [ indices U.! fromIntegral ((computeShuffledIndex i len seed)) | i <- range (start, end - 1) ] -- Careful of the upper bound not included!