{-# LANGUAGE RankNTypes #-}
module Main where

import           Data.Bits            (shiftR, (.&.), testBit)
import qualified Data.ByteString.Lazy as BSL
import           Data.Word8           (Word8)
import           Data.List.Fused      (Fused)
import qualified Data.List.Fused      as Fused

{-# INLINE fromBS #-}
fromBS :: BSL.ByteString -> Fused Word8
fromBS = Fused.fromList . BSL.unpack

{-# INLINE splitByte #-}
splitByte :: Word8 -> [Bool]
splitByte w = Prelude.map (\i-> (w `shiftR` i) .&. 1 == 1) [0..7]

{-# INLINE splitByte' #-}
splitByte' :: Word8 -> Fused Bool
splitByte' = Fused.fromList . splitByte

{-# INLINE bitStream' #-}
bitStream' :: BSL.ByteString -> Fused Bool
bitStream' = Fused.concat . Fused.map splitByte' . fromBS


{-# INLINE longestRun #-}
longestRun :: Eq a => Fused a -> Int
longestRun = maximum . Fused.map fst . Fused.runLengthEncode


main :: IO ()
main = do
    bs <- BSL.getContents
    print $ longestRun $ bitStream' bs
