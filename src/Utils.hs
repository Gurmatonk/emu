{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Lens
import Data.Bits (bit, complement, shiftL, shiftR, (.&.), (.|.), Bits (..))
import Data.Bool (bool)
import Data.Word (Word16, Word8)

boolIso :: a -> a -> Iso' Bool a
boolIso t f = iso from to
  where
    from True = t
    from False = f
    to t = True

testBitF :: (Bits a) => Int -> a -> Bool
testBitF = flip testBit

setBitF :: (Bits a) => Int -> a -> a
setBitF = flip setBit

clearBitF :: (Bits a) => Int -> a -> a
clearBitF = flip clearBit

bitwiseValue' :: Int -> Lens' Word8 Bool
bitwiseValue' b = lens getter setter
  where
    getter = flip testBit b
    setter w = bool (clearBit w b) (setBit w b)

-- flipping single bits like it's 1989 :)
-- bitwiseValue :: Word8 -> Lens' Word8 Bool
-- bitwiseValue mask = lens getter setter
--   where
--     getter w = (mask .&. w) /= 0
--     setter w = bool (w .&. complement mask) (w .|. mask)

bitTo :: Int -> Bool -> Word8 -> Word8
bitTo b v w = bool (clearBitF b w) (setBitF b w) v

dualBit :: Int -> Int -> Lens' Word8 (Bool, Bool)
dualBit upper lower = lens getter setter
  where
    getter w = (testBit w upper, testBit w lower)
    -- getter w = (w ^. bitwiseValue (bit upper), w ^. bitwiseValue (bit lower))
    setter w (u, l) = bitTo upper u . bitTo lower l $ w
    -- setter w (u, l) = w & bitwiseValue' upper .~ u & bitwiseValue' lower .~ l

mkWord16 ::
  -- | HI
  Word8 ->
  -- | LO
  Word8 ->
  Word16
mkWord16 hi lo = ((fromIntegral hi :: Word16) `shiftL` 8) + fromIntegral lo

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = (fromIntegral ((w .&. 0xFF00) `shiftR` 8), fromIntegral (w .&. 0x00FF))

swapWord8 :: Word8 -> Word8
swapWord8 w = lower + higher
  where
    lower = (w .&. 0xF0) `shiftR` 4
    higher = (w .&. 0x0F) `shiftL` 4