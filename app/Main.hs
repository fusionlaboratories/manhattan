{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Main where

-- import qualified MyLib (someFunc)
import Types
import Committee
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString.Lazy.Char8 as C
-- import Crypto.Hash.SHA256 ( hash )
import Utils
import System.Environment ( getArgs, getProgName )
import Control.Monad ( when )
import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens ( _String, key )
import Data.Maybe ( fromJust )
import Network.Wreq
import Control.Lens
import Config ( slotsPerEpoch, minSeedLookAhead, epochsPerHistoricalVector )
import Control.Exception as E
import Network.HTTP.Client ( HttpException(..), HttpExceptionContent(..) )
import Data.Ix ( range )
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import System.IO ( hFlush, stdout )

apiTokenFile :: FilePath
apiTokenFile = "api-token.json"

-- | Simple data to parse API token and endpoints
data APIToken = APIToken
  { endpoint :: String
  , token    :: String
  } deriving (Eq, Show, Generic)

instance FromJSON APIToken where
  parseJSON = withObject "APIToken" $ \v -> APIToken
    <$> v .: "endpoint"
    <*> v .: "token"

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Manhattan Test :: RANDAO election"

  -- Starting Epoch is passed as argument for now
  args <- getArgs
  when (length args < 3) $ do
    progName <- getProgName
    error $ "Usage: " ++ progName ++ " <epoch-start> <epoch-catchup> <validators-file> where\n\t<epoch-start> is the starting Epoch\n\t<epoch-catchup> is the epoch number from which the validators file was fetched\n\t<validators-file> is the path to the JSON file containing the most-recent fetched validators list"

  let startingEpoch = read (head args) :: Epoch
      startingSlot = firstSlotFromEpoch startingEpoch
      slotForInitialRandao = startingSlot - slotsPerEpoch * 1
      catchupEpoch = read (args !! 1) :: Epoch
      validatorsFile = (args !! 2)
  
  -- entries <- cmData <$> fromJust <$> decodeFileStrict "./data/committee_call_ex_1.json"
  -- print (cdeValidators (head entries))
  -- error "STOP"

  -- Parse api-token file for QuickNode access
  apiToken <- fromJust <$> decodeFileStrict apiTokenFile
  block <- fetchNextBlockFromSlot apiToken slotForInitialRandao
  validators <- vdData <$> fromJust <$> decodeFileStrict validatorsFile
  let initialRandao = prevRandao block
      -- Compute the index at which to insert the initial 
      n = (startingEpoch + epochsPerHistoricalVector - minSeedLookAhead - 1) `mod` epochsPerHistoricalVector
      initialState = LightState
        { currSlot = startingSlot
        , validators = validators
        , randaoMixes = take (fromInteger n) (repeat 0) ++ (initialRandao : take (fromInteger (epochsPerHistoricalVector - n - 1)) (repeat 0))
        }
      -- committee = getBeaconCommittee initialState startingSlot 0
  -- print committee
  runElections apiToken initialState startingEpoch catchupEpoch

  -- cm <- fetchCommitteesAtSlot apiToken startingSlot
  -- print (length cm)
  -- print (length (head cm))

-- | Recursively run elections from the starting epoch up to the catching up epoch
runElections :: APIToken -> LightState -> Epoch -> Epoch -> IO ()
runElections apiToken state epoch endEpoch = do
  putStrLn $ "Election for Epoch " ++ (show epoch) ++ "/" ++ (show endEpoch) ++ " (" ++ show (endEpoch - epoch) ++ " remaining to catchup)"
  -- Compute the fist slot of the epoch
  let slot = firstSlotFromEpoch epoch
  putStr ("\tFetching committees from chain...") >> hFlush stdout
  tic1 <- getCurrentTime
  -- Fetch all committes for all slots in this epoch
  !allCommittees <- fetchCommitteesAtSlot apiToken slot
  toc1 <- getCurrentTime
  let diff1 = diffUTCTime toc1 tic1
  putStrLn $ "(" ++ (show diff1) ++ ")"
  let committeesPerSlot = getCommitteeCountPerSlot state epoch
      count = committeesPerSlot * slotsPerEpoch
      pairs = [(s,i) | s <- range(slot, slot+slotsPerEpoch-1), i <- range(0, committeesPerSlot-1)]
  putStr ("\tComputting all " ++ (show count) ++ " committees for this epoch...") >> hFlush stdout
  tic2 <- getCurrentTime
  let !computedCommittees = concat $ map (\(slot, index) -> getBeaconCommittee state slot index) (take 20 pairs)
      !l = length computedCommittees
  toc2 <- getCurrentTime
  let diff2 = diffUTCTime toc2 tic2
  putStrLn $ "(" ++ (show diff2) ++ ")"
  putStrLn $ "\tElection passed: " ++ show (computedCommittees == concat (take 20 allCommittees))
  -- error ""


-- | Craft a QuickNode query with the endpoint and the api token
-- This is mostly a utility function
qnQuery :: APIToken -> String -> String
qnQuery d str = "https://" ++ (endpoint d) ++ ".quiknode.pro/" ++ (token d) ++ str

-- | Query the chain for the block at the given @Slot@ through QuickNode
-- If there was no block at this slot, it fetches the block from the next slot, etc.
-- until one is eventually found
fetchNextBlockFromSlot :: APIToken -> Slot -> IO LightBlock
fetchNextBlockFromSlot apiToken slot = fetchBlockAtSlot apiToken slot `E.catch` handler
  where
    fetchBlockAtSlot :: APIToken -> Slot -> IO LightBlock
    fetchBlockAtSlot apiToken slot_ = do
      r <- asJSON =<< get (qnQuery apiToken "/eth/v2/beacon/blocks/" ++ show slot_)
      return (r ^. responseBody)
    handler err@(HttpExceptionRequest _ (StatusCodeException r _))
      | r ^. responseStatus ^. statusCode == 404 = fetchBlockAtSlot apiToken (slot + 1)
      | otherwise                                = throwIO err
    handler err = throwIO err

-- | Query the chain for all committees in all slots in the epoch whose slots belongs to
-- Then, for the sake of simplicity, concatenate all indexes into a big list
fetchCommitteesAtSlot :: APIToken -> Slot -> IO [[ValidatorIndex]]
fetchCommitteesAtSlot apiToken slot = do
  cData <- fetchCommitteesAtSlot' apiToken slot `E.catch` handler
  -- return (concat $ map cdeValidators (cmData cData))
  return (map cdeValidators (cmData cData))
  where
    fetchCommitteesAtSlot' :: APIToken -> Slot -> IO CommitteeData
    fetchCommitteesAtSlot' apiToken slot_ = do
      r <- asJSON =<< get (qnQuery apiToken ("/eth/v1/beacon/states/" ++ show slot_ ++ "/committees"))
      return (r ^. responseBody)
    handler err@(HttpExceptionRequest _ _) = do
      putStrLn $ ">>> Failed to fetch committees at slot " ++ show slot ++ " from QuickNode. <<<"
      throwIO err

{-

  -- raw <- BS.readFile "/Users/nschoe/Downloads/committee_call_ex_1.json"
  -- let committeeData = decode raw :: Maybe CommitteeData
  -- putStrLn $ "Decoded: " ++ show committeeData

  -- raw <- BS.readFile "./data/one_validator.json"
  -- let validator = eitherDecode raw :: Either String Validator
  -- putStrLn $ "Validator: " ++ show validator

  -- raw <- BS.readFile "./data/all_validators.json"
  -- let (Just validatorsData) = decode raw :: Maybe ValidatorsData
      -- validators = vdData validatorsData
  -- putStrLn $ "Parsed " ++ (show (length validators)) ++ " validators."
  -- putStrLn $ show $ take 3 validators

  -- Parse validators (for now, static, from file, hand-fetched for the given epoch)
  -- raw <- BS.readFile "./data/all_validators.json"
  raw <- BS.readFile "./data/all_validators_195094.json" -- Getting a more up-to-date list of validators should not influence (due to indices)
  let (Just validatorsData) = decode raw :: Maybe ValidatorsData
      allValidators = vdData validatorsData

  let startingSlot = 6149664
      -- initialRandao = 0x80b848d9795bd5154036a4278c33c4363b0600343b964f3c127f8a93bc3271a7 -- https://etherscan.io/block/16976234#consensusinfo
      -- initialRandao = 0xb36d237e9c00bc4a1220146e1f7fee1ab6c0a412999cb1fa99661514ea432b68 -- Last randao from 2 epochs before (1 + minSeedLookAhead)
      initialRandao = 0x374677f793d1fecca4ffad2287319d2406fa7025a1b754ebefc8dedff843e37f -- Same as above, but with the hash + xor applied (so next one)
      initialState = LightState
        { currSlot   = startingSlot
        , validators = allValidators
        -- , randaoMixes = take 61103 (repeat 0) ++ (61104 : take (65536 - 61103 - 1) (repeat 0))
        , randaoMixes = take 61103 (repeat 0) ++ (initialRandao : take (65536 - 61103 - 1) (repeat 0))
        }
      epoch = epochFromSlot (currSlot initialState)

  putStr $ "Initial slot: " ++ show (currSlot initialState)
  putStrLn $ " (epoch: " ++ show epoch ++ ")"
  putStr $ "Nb of validators: " ++ show (length (validators initialState))
  putStrLn $ " (" ++ show (length (getActiveValidatorIndices initialState epoch)) ++ " active for this epoch)"
  -- First check if we can find the committee for the starting slot
  let computedCommittee = getBeaconCommittee initialState startingSlot 1
  putStrLn $ "Computed committee (" ++ show (length computedCommittee) ++ " members): " ++ show computedCommittee
  
--   putStrLn $ "Computed proposer index: " ++ show (getBeaconProposerIndex initialState)
--   let bstr = BS.pack [1, 0, 0, 0, 252, 150, 0, 0, 0, 0, 0, 0, 49, 29, 74, 22, 244, 77, 146, 10, 137, 181, 24, 34, 54, 181, 252, 139, 118, 78, 89, 239, 168, 84, 170, 151, 40, 22, 241, 164, 19, 186, 31, 47]

  -- let committeeIdx = 111111
      -- val = (validators initialState) !! committeeIdx
  -- putStrLn $ "Committee @ " ++ show committeeIdx ++ ": is active (" ++ show (isActiveValidator val epoch) ++ ")\n" ++ show val
  -}