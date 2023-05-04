module Config ( slotsPerEpoch
              , maxCommitteesPerSlot
              , targetCommitteeSize
              , maxValidatorsPerCommittee
              , epochsPerHistoricalVector
              , minSeedLookAhead
              , shuffleRoundCount
              , maxEffectiveBalance
               ) where

import Data.Word ( Word64 )

slotsPerEpoch :: Word64
slotsPerEpoch = 32

maxCommitteesPerSlot :: Word64
maxCommitteesPerSlot = 64 -- 2^6

targetCommitteeSize :: Word64
targetCommitteeSize = 128 -- 2^7

maxValidatorsPerCommittee :: Word64
maxValidatorsPerCommittee = 2048 -- 2^11

shuffleRoundCount :: Word64
shuffleRoundCount = 90

-- | Unit: Epochs
epochsPerHistoricalVector :: Word64
epochsPerHistoricalVector = 65536 -- 2^16

-- | Unit: Epochs
minSeedLookAhead :: Word64
minSeedLookAhead = 1 

maxEffectiveBalance :: Word64
maxEffectiveBalance = 32000000000