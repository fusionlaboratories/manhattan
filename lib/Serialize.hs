module Serialize ( serializeInteger
                 , unserializeByteString
                 ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Control.Exception ( assert )

-- | Serialize an integer into a ByteString (little Endian), with optional padding to get the number of bytes
serializeInteger :: Int -> Integer -> ByteString
serializeInteger n i = let str = serializeInteger' i
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