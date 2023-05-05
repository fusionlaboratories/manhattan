{-# LANGUAGE BangPatterns      #-}

module Utils ( epochFromSlot
             , firstSlotFromEpoch
             , getCommitteeCountPerSlot
             , domainTypeValues
             , getRandaoMix
             , getSeed
             , computeShuffledIndex
             , isActiveValidator
             , getActiveValidatorIndices
            --  , computeProposerIndex
            --  , mySR
            --  , mySRB
             ) where

import Types
import Config
import Crypto.Hash.SHA256 ( hash )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS ( index, take, append, pack )
-- import qualified Data.ByteString.Char8 as B
import Control.Exception ( assert )
import Data.Word ( Word64 )
import Data.Bits ( testBit )
import Serialize ( serializeWord64, unserializeByteStringToWord64 )
import Debug.Trace ( trace )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | Compute the Epoch a slot lived in
epochFromSlot :: Slot -> Epoch
epochFromSlot slot = slot `div` slotsPerEpoch

-- | Given a Epoch, returns its first slot
firstSlotFromEpoch :: Epoch -> Slot
firstSlotFromEpoch epoch = epoch * slotsPerEpoch

-- | Return the number of committees in each slot for the given epoch
getCommitteeCountPerSlot :: LightState -> Epoch -> Word64
getCommitteeCountPerSlot state epoch = max 1 $ min maxCommitteesPerSlot (l `div` slotsPerEpoch `div` targetCommitteeSize)
    -- where l = (toInteger . length . validatorIndexes) state
    where l = fromIntegral (U.length (getActiveValidatorIndices state epoch))

-- domainTypeValues :: DomainType -> Word32
-- domainTypeValues :: DomainType -> Integer
domainTypeValues :: DomainType -> ByteString
domainTypeValues DOMAIN_BEACON_PROPOSER     = BS.pack [0x00, 0x00, 0x00, 0x00]
domainTypeValues DOMAIN_BEACON_ATTESTER     = BS.pack [0x01, 0x00, 0x00, 0x00]
domainTypeValues DOMAIN_RANDAO              = BS.pack [0x02, 0x00, 0x00, 0x00]
domainTypeValues DOMAIN_DEPOSIT             = BS.pack [0x03, 0x00, 0x00, 0x00]
domainTypeValues DOMAIN_VOLUNTARY_EXIT      = BS.pack [0x04, 0x00, 0x00, 0x00]
domainTypeValues DOMAIN_SELECTION_PROOF     = BS.pack [0x05, 0x00, 0x00, 0x00]
domainTypeValues DOMAIN_AGGREGATE_AND_PROOF = BS.pack [0x06, 0x00, 0x00, 0x00]
domainTypeValues DOMAIN_APPLICATION_MASK    = BS.pack [0x00, 0x00, 0x00, 0x01]

-- | Return the seed for a given epoch
-- getSeed :: LightState -> Epoch -> DomainType -> Word256
getSeed :: LightState -> Epoch -> DomainType -> ByteString -- TEMPORARY, get types okay then goes back to Word256
getSeed state epoch domain =
    -- let mix = serializeWord64 (getRandaoMix state (epoch + epochsPerHistoricalVector - minSeedLookAhead - 1)) 32
    -- in hash $ (serializeWord64 (domainTypeValues domain) 4) `BS.append` (serializeWord64 epoch 8) `BS.append` mix
    -- CRAZY (I don't get it) weird big vs littel andian manipulations (based on Tobia's data!)
    let mix = getRandaoMix state (epoch + epochsPerHistoricalVector - minSeedLookAhead - 1)
        preimage = domainTypeValues domain `BS.append` serializeWord64 8 epoch `BS.append` mix
    in hash preimage


-- | Return the randao mix at a recent epoch
-- getRandaoMix :: LightState -> Epoch -> Word256
-- getRandaoMix :: LightState -> Epoch -> Integer
getRandaoMix :: LightState -> Epoch -> ByteString
getRandaoMix state epoch = let n = epoch `mod` epochsPerHistoricalVector
                           in mixes V.! (fromIntegral n)
    where mixes = randaoMixes state

-- | Return the shuffled index corresponding to seed and indexCount
-- NOTE: this function was tested against test data gathered by Tobias and it returned the right answer.
-- So I think this implementation is good and should not move: error is elsewhere.
computeShuffledIndex :: Word64 -> Word64 -> ByteString -> Word64
-- computeShuffledIndex i ic s = trace ("\t\t\tShuffling: " ++ show shuffleRoundCount ++ " times") (swapOrNotRound 0 shuffleRoundCount i ic s)
computeShuffledIndex = swapOrNotRound 0 shuffleRoundCount
-- computeShuffledIndex = swapOrNotRound 0 shuffleRoundCount
    where swapOrNotRound :: Word64 -> Word64 -> Word64 -> Word64 -> ByteString -> Word64
          swapOrNotRound _ 0 index _ _ = index
          swapOrNotRound !currentRound !remainingRounds !index !indexCount_ !seed =
            let indexCount = assert (index < indexCount_) indexCount_
                pivot = unserializeByteStringToWord64 (BS.take 8 (hash (seed `BS.append` (serializeWord64 1 currentRound)))) `mod` indexCount
                flipP = (pivot + indexCount -  index) `mod` indexCount
                position = max index flipP
                posAsBytes = serializeWord64 4 (position `div` 256)
                source = hash (seed `BS.append` (serializeWord64 1 currentRound) `BS.append` posAsBytes)
                byte = BS.index source (fromIntegral ((position `mod` 256) `div` 8))
                newIndex = if (testBit byte (fromIntegral (position `mod` 8))) then flipP else index
            in swapOrNotRound (currentRound+1) (remainingRounds-1) newIndex indexCount_ seed

-- | Returns whether a validator is active for the given epoch
isActiveValidator :: Validator -> Epoch -> Bool
isActiveValidator validator epoch =
    activationEpoch validator <= epoch && epoch < exitEpoch validator

-- | Returns the list of active validators (their indices) for the given epoch
getActiveValidatorIndices :: LightState -> Epoch -> U.Vector ValidatorIndex
getActiveValidatorIndices state epoch = flip U.imapMaybe (validators state) $ \i v ->
    if isActiveValidator v epoch then Just (fromIntegral i) else Nothing
-- getActiveValidatorIndices state epoch = V.fromList $ reverse $ filterByValidity epoch (validators state) [] 0
--     where filterByValidity :: Epoch -> [Validator] -> [ValidatorIndex] -> ValidatorIndex -> [ValidatorIndex]
--           filterByValidity _ [] is _ = is
--           filterByValidity epoch_ (v:vs) is !i | isActiveValidator v epoch_ = filterByValidity epoch_ vs (i:is) (i+1)
--                                                | otherwise                  = filterByValidity epoch_ vs is (i+1)




-- | Compute the proposer index from the list of active validators, sampled by the effective balance
-- computeProposerIndex :: LightState -> [ValidatorIndex] -> ByteString -> ValidatorIndex
-- computeProposerIndex _ [] _ = error "Can't chose a proposer index from empty list!"
-- computeProposerIndex state indices seed =
--     let maxRandomByte = 2^8 - 1
--         total = length indices
--     in go state indices total seed maxRandomByte 0
--     where go :: LightState -> [ValidatorIndex] -> Int -> ByteString -> Integer -> Integer -> ValidatorIndex
--           go state indices total seed maxRandomByte i =
--             let candidateIndex = indices !! fromInteger ((computeShuffledIndex (i `mod` (toInteger total)) total seed))
--                 randomByte = BS.index (hash (seed `BS.append` (serializeWord64 8 (i `div` 32)))) (fromInteger $ i `mod` 32)
--                 effectiveBal = effectiveBalance $ (validators state) !! (fromInteger candidateIndex)
--             in if (effectiveBal * maxRandomByte >= maxEffectiveBalance * (toInteger randomByte))
--                 then candidateIndex
--                 else go state indices total seed maxRandomByte (i+1)
