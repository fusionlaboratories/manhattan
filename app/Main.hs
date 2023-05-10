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
import Control.Monad ( when, forM_ )
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

--{-
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

-- | Recursively run elections from the starting epoch up to the catching up epoch
runElections :: APIToken -> LightState -> Epoch -> Epoch -> IO ()
runElections apiToken state epoch endEpoch = do
  putStrLn $ "Election for Epoch " ++ (show epoch) ++ "/" ++ (show endEpoch) ++ " (" ++ show (endEpoch - epoch) ++ " remaining to catchup)"
  -- Compute the fist slot of the epoch
  let slot = firstSlotFromEpoch epoch
  -- putStr ("\tFetching committees from chain...") >> hFlush stdout
  putStr ("\tFetching committees from file...") >> hFlush stdout
  tic1 <- getCurrentTime
  -- Fetch all committes for all slots in this epoch
  -- allCommittees <- fetchCommitteesAtSlot apiToken slot
  !allCommittees <- fetchCommitteesFromFile "./data/committee_call_ex_1.json"
  toc1 <- getCurrentTime
  let diff1 = diffUTCTime toc1 tic1
  putStrLn $ "(" ++ (show diff1) ++ ")"

  putStr ("\tComputing list of active validators for epoch " ++ show epoch ++ "...") >> hFlush stdout
  tic2 <- getCurrentTime
  !activeIndices <- getActiveValidatorIndices state epoch
  let !len = fromIntegral (MV.length activeIndices)
  toc2 <- getCurrentTime
  let diff2 = diffUTCTime toc2 tic2
  putStrLn $ "(" ++ (show diff2) ++ ")"

  committeesPerSlot <- getCommitteeCountPerSlot state epoch
  let count = committeesPerSlot * slotsPerEpoch
  
  putStrLn ("\tComputting all " ++ (show count) ++ " committees for this epoch...")
  tic3 <- getCurrentTime
  -- indexSum <- MV.new 1
  -- MV.set indexSum 0

  computedCommittees <- do
    v <- M.new (fromIntegral (slotsPerEpoch * committeesPerSlot))
    forM_ (range (slot, slot+slotsPerEpoch-1)) $ \s ->
      forM_ (range (0, committeesPerSlot-1)) $ \i -> do
        !committee <- getBeaconCommittee state activeIndices len s i
        M.write v (fromIntegral ((s - slot) * committeesPerSlot + i)) committee
        -- val' <- MV.foldl' (+) 0 committee
        -- MV.modify indexSum (\val -> val + val') 0
    return v
  
  toc3 <- getCurrentTime
  let !diff3 = diffUTCTime toc3 tic3
  putStrLn $ "\tComputation done in " ++ (show diff3)

  -- Probably stupid way to test (accumulate) comparison to test election result
  equals <- MV.new 1
  MV.set equals True

  tic4 <- getCurrentTime
  M.iforM_ computedCommittees $ \i com -> do
    let targetCom = allCommittees V.! i
    MV.iforM_ com $ \j comMember -> do
      let targetMem = targetCom U.! j
          areEqual = comMember == targetMem
      MV.modify equals (\eq -> eq && areEqual) 0
  
  toc4 <- getCurrentTime
  let diff4 = diffUTCTime toc4 tic4

  isCorrect <- MV.read equals 0
  putStrLn $ "Election is correct: " ++ show isCorrect ++ " (in " ++ show diff4 ++ ")"


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
  cData <- fetchCommitteesAtSlot' apiToken slot `E.catch` handler
  -- return (concat $ map cdeValidators (cmData cData))
  return $ V.fromList (map cdeValidators (cmData cData))
  where
    fetchCommitteesAtSlot' :: APIToken -> Slot -> IO CommitteeData
    fetchCommitteesAtSlot' apiToken slot_ = do
      r <- asJSON =<< get (qnQuery apiToken ("/eth/v1/beacon/states/" ++ show slot_ ++ "/committees"))
      return (r ^. responseBody)
    handler (HttpExceptionRequest _ ResponseTimeout) = do
            putStrLn "QuickNode API request timed out, retrying in 10 s..."
            threadDelay (10 * 1000000)
            fetchCommitteesAtSlot' apiToken slot
    handler (HttpExceptionRequest _ ConnectionTimeout) = do
            putStrLn "QuickNode APIconnection timed out, retrying in 60 s..."
            threadDelay (60 * 1000000)
            fetchCommitteesAtSlot' apiToken slot
    handler err@(HttpExceptionRequest _ _) = do
            putStrLn $ ">>> Failed to fetch committees at slot " ++ show slot ++ " from QuickNode. <<<"
            throwIO err
    handler err = do
      putStrLn $ "Unrecoverable error:"
      throwIO err


-- | Function used for quicker prototyping: get a set of committees for a given slot from a file
-- instead of making a API call
-- fetchCommitteesFromFile :: FilePath -> IO [[ValidatorIndex]]
fetchCommitteesFromFile :: FilePath -> IO (V.Vector (U.Vector ValidatorIndex))
fetchCommitteesFromFile file = do
  cData <- fromJust <$> decodeFileStrict file
  return $ V.fromList (map cdeValidators (cmData cData))