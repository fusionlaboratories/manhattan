{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Main where

import Types
import Committee
import Data.Aeson
import qualified Data.ByteString as BS
import Utils
import System.Environment ( getArgs, getProgName )
import Control.Monad ( when, forM_, unless )
import GHC.Generics ( Generic )
import Data.Maybe ( fromJust )
import Network.Wreq
    ( asJSON, get, responseBody, responseStatus, statusCode )
import Control.Lens
import Config ( slotsPerEpoch, minSeedLookAhead, epochsPerHistoricalVector )
import Control.Exception as E ( catch, throwIO )
import Network.HTTP.Client ( HttpException(..), HttpExceptionContent(..) )
import Data.Ix ( range )
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import System.IO ( hFlush, stdout )
import Control.Concurrent ( threadDelay )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Mutable as M

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

{-
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

  -- Parse api-token file for QuickNode access
  apiToken <- fromJust <$> decodeFileStrict apiTokenFile
  block <- fetchNextBlockFromSlot apiToken slotForInitialRandao
  validators_ <- vdData <$> fromJust <$> decodeFileStrict validatorsFile
  let initialRandao = prevRandao block
      -- Compute the index at which to insert the initial 
      n = (startingEpoch + epochsPerHistoricalVector - minSeedLookAhead - 1) `mod` epochsPerHistoricalVector
      initialState = LightState
        { currSlot = startingSlot
        , validators = validators_
        , randaoMixes = (V.replicate (fromIntegral n) BS.empty) V.++ (initialRandao `V.cons` (V.replicate (fromIntegral (epochsPerHistoricalVector - n - 1)) BS.empty))
        }
  runElections apiToken initialState startingEpoch catchupEpoch
---}

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
      catchupEpoch = read (args !! 1) :: Epoch
      validatorsFile = (args !! 2)

  -- Parse validators from file passed as argument
  validators_ <- vdData <$> fromJust <$> decodeFileStrict validatorsFile

  let initialState = LightState
        { currSlot = startingSlot
        , validators = validators_
        , randaoMixes = V.replicate (fromIntegral epochsPerHistoricalVector) BS.empty
        }

  -- Parse api-token file for QuickNode access
  apiToken <- fromJust <$> decodeFileStrict apiTokenFile

  runElections apiToken initialState startingEpoch catchupEpoch

  -- block <- fetchNextBlockFromSlot apiToken slotForInitialRandao
  
  -- let initialRandao = prevRandao block
      -- Compute the index at which to insert the initial 
      -- n = (startingEpoch + epochsPerHistoricalVector - minSeedLookAhead - 1) `mod` epochsPerHistoricalVector
      -- initialState = LightState
        -- { currSlot = startingSlot
        -- , validators = validators_
        -- , randaoMixes = (V.replicate (fromIntegral n) BS.empty) V.++ (initialRandao `V.cons` (V.replicate (fromIntegral (epochsPerHistoricalVector - n - 1)) BS.empty))
        -- }
  runElections apiToken initialState startingEpoch catchupEpoch

-- | Recursively run elections from the starting epoch up to the catching up epoch
runElections :: APIToken -> LightState -> Epoch -> Epoch -> IO ()
runElections apiToken state' !epoch endEpoch = when (epoch <= endEpoch) $ do
  putStrLn $ "Election for Epoch " ++ (show epoch) ++ "/" ++ (show endEpoch) ++ " (" ++ show (endEpoch - epoch) ++ " remaining to catchup)"

  -- Compute the fist slot of the epoch
  let slot = firstSlotFromEpoch epoch
      firstSlot = firstSlotFromEpoch epoch
      slotForRandaoMix = firstSlot - slotsPerEpoch * 1

  putStrLn "\tFetching randao seed from chain..."
  -- Fetch corresponding block to get the randao mix
  randaoMix <- prevRandao <$> fetchNextBlockFromSlot apiToken slotForRandaoMix

  -- Compute index at which this new randaoMix goes
  let n = (epoch + epochsPerHistoricalVector - minSeedLookAhead - 1) `mod` epochsPerHistoricalVector

  -- Insert the randao mix in the state
  let !state = state' {
    randaoMixes = (V.//) (randaoMixes state') [(fromIntegral n, randaoMix)]
  }

  -- Fetch actual, elected committee from the chain (to match against, this is just verification)
  putStrLn "\tFetching committees from chain..."
  -- putStr ("\tFetching committees from file...") >> hFlush stdout

  -- tic1 <- getCurrentTime
  -- Fetch all committes for all slots in this epoch
  allCommittees <- fetchCommitteesAtSlot apiToken slot
  -- !allCommittees <- fetchCommitteesFromFile "./data/committee_call_ex_1.json"
  -- toc1 <- getCurrentTime
  -- let diff1 = diffUTCTime toc1 tic1
  -- putStrLn $ "(" ++ (show diff1) ++ ")"

  putStr ("\tComputing list of active validators for epoch " ++ show epoch ++ "...") >> hFlush stdout
  tic2 <- getCurrentTime
  !activeIndices <- getActiveValidatorIndices state epoch
  let !len = fromIntegral (MV.length activeIndices)
  toc2 <- getCurrentTime
  let diff2 = diffUTCTime toc2 tic2
  putStrLn $ "(" ++ (show diff2) ++ ")"

  committeesPerSlot <- getCommitteeCountPerSlot state epoch
  let count = fromIntegral $ committeesPerSlot * slotsPerEpoch

  putStrLn ("\tComputting all " ++ (show count) ++ " committees for this epoch...")
  tic3 <- getCurrentTime

  -- DIRTY and TEMPORARY trick to shuffle only once
  isShuffled <- MV.new 1
  -- MV.set isShuffled False
  MV.set isShuffled 1

-- Copy the active indices BEFORE passing it to getBeaconCommittee, doh!
  copyIndices <- MV.clone activeIndices

  computedCommittees <- do
    v <- M.new count
    forM_ (range (slot, slot+slotsPerEpoch-1)) $ \s ->
      forM_ (range (0, committeesPerSlot-1)) $ \i -> do
        -- putStrLn $ "(" ++ show s ++ ", " ++ show i ++ ") -> " ++ show ((s - slot) * committeesPerSlot + i) ++ ")"
        -- !committee <- getBeaconCommittee isShuffled state activeIndices len s i
        !committee <- getBeaconCommittee isShuffled state copyIndices len s i
        M.write v (fromIntegral ((s - slot) * committeesPerSlot + i)) committee
    return v

  toc3 <- getCurrentTime
  let !diff3 = diffUTCTime toc3 tic3
  putStrLn $ "\tComputation done in " ++ (show diff3)

  -- Probably stupid way to test (accumulate) comparison to test election result
  equals <- MV.new 1
  MV.set equals True

  -- DEBUG: just to count number of wrongly swapped indices
  -- comps <- MV.new 2
  -- MV.set comps (0 :: Int)

  M.iforM_ computedCommittees $ \i com -> do
    let targetCom = allCommittees V.! i
    MV.iforM_ com $ \j comMember -> do
      let targetMem = targetCom U.! j
          areEqual = comMember == targetMem
          z = if areEqual then 0 else 1
      -- unless areEqual $ do
        -- putStrLn $ "com# " ++ show i ++ ", mem# " ++ show j ++ " are DIFFERENT"
      -- MV.modify comps (\val -> val + 1) z
      MV.modify equals (\eq -> eq && areEqual) 0

  isCorrect <- MV.read equals 0
  putStrLn $ "\tElection (for Epoch " ++ show epoch ++ ") is correct: " ++ show isCorrect
  putStrLn ""

  runElections apiToken state (epoch+1) endEpoch
  
  -- nbEq <- MV.read comps 0
  -- nbDiff <- MV.read comps 1

  -- putStrLn $ "\tNB equals: " ++ show nbEq
  -- putStrLn $ "\tNB diffs: " ++ show nbDiff

  -- let m = 100
  -- cOne <- M.read computedCommittees m
  -- putStr $ "computed[" ++ show m ++ "]: "
  -- MV.forM_ cOne $ \member -> putStr $ show member ++ ", "
  -- putStrLn ""

  -- let aOne = allCommittees V.! m
  -- putStrLn $ "all[" ++ show m ++ "]: " ++ show aOne

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
-- fetchCommitteesAtSlot :: APIToken -> Slot -> IO [[ValidatorIndex]]
fetchCommitteesAtSlot :: APIToken -> Slot -> IO (V.Vector (U.Vector ValidatorIndex))
fetchCommitteesAtSlot apiToken slot = do
  cData <- fetchCommitteesAtSlot' slot `E.catch` handler
  -- return (concat $ map cdeValidators (cmData cData))
  return $ V.fromList (map cdeValidators (cmData cData))
  where
    fetchCommitteesAtSlot' :: Slot -> IO CommitteeData
    fetchCommitteesAtSlot' slot_ = do
      r <- asJSON =<< get (qnQuery apiToken ("/eth/v1/beacon/states/" ++ show slot_ ++ "/committees"))
      return (r ^. responseBody)
    handler (HttpExceptionRequest _ ResponseTimeout) = do
            putStrLn "\tQuickNode API request timed out, retrying in 10 s..."
            threadDelay (10 * 1000000)
            fetchCommitteesAtSlot' slot
    handler (HttpExceptionRequest _ ConnectionTimeout) = do
            putStrLn "\tQuickNode APIconnection timed out, retrying in 60 s..."
            threadDelay (60 * 1000000)
            fetchCommitteesAtSlot' slot
    handler err@(HttpExceptionRequest _ _) = do
            putStrLn $ "\t>>> Failed to fetch committees at slot " ++ show slot ++ " from QuickNode. <<<"
            throwIO err
    handler err = do
      putStrLn $ "\tUnrecoverable error:"
      throwIO err


-- | Function used for quicker prototyping: get a set of committees for a given slot from a file
-- instead of making a API call
-- fetchCommitteesFromFile :: FilePath -> IO [[ValidatorIndex]]
fetchCommitteesFromFile :: FilePath -> IO (V.Vector (U.Vector ValidatorIndex))
fetchCommitteesFromFile file = do
  cData <- fromJust <$> decodeFileStrict file
  return $ V.fromList (map cdeValidators (cmData cData))