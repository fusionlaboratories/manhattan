module Config ( slotsPerEpoch
              , maxCommitteesPerSlot
              , targetCommitteeSize
              , maxValidatorsPerCommittee
              , epochsPerHistoricalVector
              , minSeedLookAhead
              , shuffleRoundCount
              , maxEffectiveBalance
               ) where

-- import Data.Word

-- slotsPerEpoch :: Word64
slotsPerEpoch :: Integer
slotsPerEpoch = 32

-- maxCommitteesPerSlot :: Word64
maxCommitteesPerSlot :: Integer
maxCommitteesPerSlot = 64 -- 2^6

-- targetCommitteeSize :: Word64
targetCommitteeSize :: Integer
targetCommitteeSize = 128 -- 2^7

-- maxValidatorsPerCommittee :: Word64
maxValidatorsPerCommittee :: Integer
maxValidatorsPerCommittee = 2048 -- 2^11

-- shuffleRoundCount :: Word64
shuffleRoundCount :: Integer
shuffleRoundCount = 90

-- | Unit: Epochs
-- epochsPerHistoricalVector :: Word64
epochsPerHistoricalVector :: Integer
epochsPerHistoricalVector = 65536 -- 2^16

-- | Unit: Epochs
-- minSeedLookAhead :: Word64
minSeedLookAhead :: Integer
minSeedLookAhead = 1 

maxEffectiveBalance :: Integer
maxEffectiveBalance = 32000000000