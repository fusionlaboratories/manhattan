module Serialize ( serializeInteger
                 , unserializeByteString
                 ) where

import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Word ( Word8, Word64 )
import Control.Exception ( assert )
import Data.List ( foldl' )

-- | Serialize an integer into a ByteString (little Endian), with optional padding to get the number of bytes
serializeInteger :: Int -> Word64 -> ByteString
serializeInteger n i = let str = serializeInteger' i
                           len = BS.length str
                           diff = assert (len <= n) (n - len)
                       in BS.append str $ BS.replicate diff 0x00

serializeInteger' :: Word64 -> ByteString
serializeInteger' 0 = BS.empty
serializeInteger' i = ((fromIntegral i) :: Word8) `BS.cons` serializeInteger' (i `div` 256) -- 256 = 2^8

unserializeByteString :: ByteString -> Word64
unserializeByteString bs = let powers = take (BS.length bs) [2^n | n <- [0,8..]]
                               ints = map fromIntegral (BS.unpack bs)
                               sums = zipWith (*) powers ints
                           in foldl' (+) 0 sums