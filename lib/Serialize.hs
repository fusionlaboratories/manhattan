module Serialize ( serializeWord64
                 , unserializeByteStringToWord64
                 , serializeInteger
                 ) where

import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Word ( Word8, Word64 )
import Control.Exception ( assert )
import Data.List ( foldl' )

-- | Serialize an integer into a ByteString (little Endian), with optional padding to get the number of bytes
-- serializeWord64 :: Integral a => Int -> a -> ByteString
serializeWord64 :: Int -> Word64 -> ByteString
serializeWord64 n i = let str = serializeWord64' i
                          len = BS.length str
                          diff = assert (len <= n) (n - len)
                      in BS.append str $ BS.replicate diff 0x00

-- serializeWord64' :: Integral a => a -> ByteString
serializeWord64' :: Word64 -> ByteString
serializeWord64' 0 = BS.empty
-- serializeWord64' i = ((fromInteger i) :: Word8) `BS.cons` serializeWord64' (i `div` 256) -- 256 = 2^8
serializeWord64' i = (fromIntegral i :: Word8) `BS.cons` serializeWord64' (i `div` 256) -- 256 = 2^8

-- unserializeByteStringToWord64 :: Integral a => ByteString -> a
unserializeByteStringToWord64 :: ByteString -> Word64
unserializeByteStringToWord64 bs = let powers = take (BS.length bs) [2^n | n <- [0,8..]]
                                       ints = map fromIntegral (BS.unpack bs)
                                       sums = zipWith (*) powers ints
                                    in foldl' (+) 0 sums

-- Below is only used to parse prev randao values (MUST DO BETTER)
serializeInteger :: Int -> Integer -> ByteString
serializeInteger n i = let str = serializeInteger' i
                           len = BS.length str
                           diff = assert (len <= n) (n - len)
                       in BS.append str $ BS.replicate diff 0x00

serializeInteger' :: Integer -> ByteString
serializeInteger' 0 = BS.empty
serializeInteger' i = (fromInteger i :: Word8) `BS.cons` serializeInteger' (i `div` 256) -- 256 = 2^8