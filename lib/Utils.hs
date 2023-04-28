module Utils ( epochFromSlot
             , firstSlotFromEpoch
             , getCommitteeCountPerSlot
             , domainTypeValues
             , getRandaoMix
             , getSeed
             , computeShuffledIndex
             , isActiveValidator
             , getActiveValidatorIndices
             , serializeInteger
             , unserializeByteString
             , computeProposerIndex
            --  , mySR
            --  , mySRB
             ) where

import Types
import Config
import Crypto.Hash.SHA256 ( hash )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS ( index, cons, empty, unpack, length, takeEnd, reverse, take, append, pack, replicate )
-- import qualified Data.ByteString.Char8 as B
import Control.Exception ( assert )
import Data.Word ( Word8 )
import Data.Bits ( testBit, shiftR )
import System.IO.Unsafe ( unsafePerformIO )

-- | Compute the Epoch a slot lived in
epochFromSlot :: Slot -> Epoch
epochFromSlot slot = slot `div` slotsPerEpoch

-- | Given a Epoch, returns its first slot
firstSlotFromEpoch :: Epoch -> Slot
firstSlotFromEpoch epoch = epoch * slotsPerEpoch

-- | Return the number of committees in each slot for the given epoch
-- getCommitteeCountPerSlot :: LightState -> Epoch -> Word64
getCommitteeCountPerSlot :: LightState -> Epoch -> Integer
getCommitteeCountPerSlot state epoch = max 1 $ min maxCommitteesPerSlot (l `div` slotsPerEpoch `div` targetCommitteeSize)
    -- where l = (toInteger . length . validatorIndexes) state
    where l = toInteger (length (getActiveValidatorIndices state epoch))

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
    -- let mix = serializeInteger (getRandaoMix state (epoch + epochsPerHistoricalVector - minSeedLookAhead - 1)) 32
    -- in hash $ (serializeInteger (domainTypeValues domain) 4) `BS.append` (serializeInteger epoch 8) `BS.append` mix
    -- CRAZY (I don't get it) weird big vs littel andian manipulations (based on Tobia's data!)
    let mix = getRandaoMix state (epoch + epochsPerHistoricalVector - minSeedLookAhead - 1)
        preimage = (domainTypeValues domain) `BS.append` (serializeInteger epoch 8) `BS.append` mix
    in hash preimage
              

-- | Return the randao mix at a recent epoch
-- getRandaoMix :: LightState -> Epoch -> Word256
-- getRandaoMix :: LightState -> Epoch -> Integer
getRandaoMix :: LightState -> Epoch -> ByteString
getRandaoMix state epoch = let n = epoch_ `mod` epochsPerHistoricalVector
                           in mixes !! (fromInteger n)
    where epoch_ = assert (epoch < 2^64) epoch
          mixes = assert (toInteger (length (randaoMixes state)) == epochsPerHistoricalVector) (randaoMixes state)

-- | Serialize an integer into a ByteString (little Endian), with optional padding to get the number of bytes
serializeInteger :: Integer -> Int -> ByteString
serializeInteger i n = let str = serializeInteger' i
                           len = BS.length str
                           diff = assert (len <= n) (n - len)
                       in BS.append str $ BS.replicate diff 0x00

serializeInteger' :: Integer -> ByteString
serializeInteger' 0 = BS.empty
serializeInteger' i | i < 0     = error $ "Serialize is only supported for positive numbers, " ++ show i ++ " is not."
                   | otherwise = ((fromInteger i) :: Word8) `BS.cons` serializeInteger' (i `div` 256) -- 256 = 2^8

unserializeByteString :: ByteString -> Integer
unserializeByteString bs = let powers = take (BS.length bs) [2^n | n <- [0,8..]]
                               ints = map toInteger (BS.unpack bs)
                               sums = zipWith (*) powers ints
                           in foldl (+) 0 sums

-- | Return the shuffled index corresponding to seed and indexCount
-- NOTE: this function was tested against test data gathered by Tobias and it returned the right answer.
-- So I think this implementation is good and should not move: error is elsewhere.
computeShuffledIndex :: Integer -> Int -> ByteString -> Integer
computeShuffledIndex = swapOrNotRound 0 shuffleRoundCount
    where swapOrNotRound :: Integer -> Integer -> Integer -> Int -> ByteString -> Integer
          swapOrNotRound _ 0 index _ _ = index
          swapOrNotRound currentRound remainingRounds index indexCount_ seed =
            let indexCount = assert (index < (toInteger indexCount_)) (toInteger indexCount_)
                -- QUESTION: is this `BS.take 8` or `BS.takeEnd 8` ?
                pivot = unserializeByteString(BS.take 8 (hash (seed `BS.append` (serializeInteger currentRound 1)))) `mod` indexCount
                flipP = (pivot + indexCount - index) `mod` indexCount
                position = max index flipP
                -- source = hash $ serializeInteger $ (unserializeByteString seed) + currentRound + (position `div` 256)
                -- posAsBytes = BS.append (BS.pack [0x00, 0x00]) (serializeInteger (position `div` 256) 4)
                posAsBytes = serializeInteger (position `div` 256) 4
                -- source = hash $ BS.append seed (BS.append (serializeInteger currentRound 1) posAsBytes)
                source = hash (seed `BS.append` (serializeInteger currentRound 1) `BS.append` posAsBytes)
                -- 'source' is a string of bytes, do we take from the end of 'source' (BS.reverse)?
                byte = BS.index source (fromInteger ((position `mod` 256) `div` 8))
                newIndex = if (testBit byte (fromInteger (position `mod` 8))) then flipP else index
            in swapOrNotRound (currentRound+1) (remainingRounds-1) newIndex indexCount_ seed

-- | Returns whether a validator is active for the given epoch
isActiveValidator :: Validator -> Epoch -> Bool
isActiveValidator validator epoch =
    activationEpoch validator <= epoch && epoch < exitEpoch validator

-- | Returns the list of active validators (their indices) for the given epoch
getActiveValidatorIndices :: LightState -> Epoch -> [ValidatorIndex]
getActiveValidatorIndices state epoch = reverse $ filterByValidity epoch (validators state) [] 0
    where filterByValidity :: Epoch -> [Validator] -> [ValidatorIndex] -> ValidatorIndex -> [ValidatorIndex]
          filterByValidity _ [] is _ = is
          filterByValidity epoch_ (v:vs) is i | isActiveValidator v epoch_ = filterByValidity epoch_ vs (i:is) (i+1)
                                              | otherwise                  = filterByValidity epoch_ vs is (i+1)

-- | Compute the proposer index from the list of active validators, sampled by the effective balance
computeProposerIndex :: LightState -> [ValidatorIndex] -> ByteString -> ValidatorIndex
computeProposerIndex _ [] _ = error "Can't chose a proposer index from empty list!"
computeProposerIndex state indices seed =
    let maxRandomByte = 2^8 - 1
        total = length indices
    in go state indices total seed maxRandomByte 0
    where go :: LightState -> [ValidatorIndex] -> Int -> ByteString -> Integer -> Integer -> ValidatorIndex
          go state indices total seed maxRandomByte i =
            let candidateIndex = indices !! fromInteger ((computeShuffledIndex (i `mod` (toInteger total)) total seed))
                randomByte = BS.index (hash (seed `BS.append` (serializeInteger (i `div` 32) 8))) (fromInteger $ i `mod` 32)
                effectiveBal = effectiveBalance $ (validators state) !! (fromInteger candidateIndex)
            in if (effectiveBal * maxRandomByte >= maxEffectiveBalance * (toInteger randomByte))
                then candidateIndex
                else go state indices total seed maxRandomByte (i+1)
