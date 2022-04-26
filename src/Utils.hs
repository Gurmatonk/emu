{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Lens
import Data.Bits (bit, complement, shiftL, shiftR, (.&.), (.|.))
import Data.Bool (bool)
import Data.Word (Word16, Word8)

boolIso :: a -> a -> Iso' Bool a
boolIso t f = iso from to
  where
    from True = t
    from False = f
    to t = True

-- flipping single bits like it's 1989 :)
bitwiseValue :: Word8 -> Lens' Word8 Bool
bitwiseValue mask = lens getter setter
  where
    getter w = (mask .&. w) > 0
    setter w = bool (w .&. complement mask) (w .|. mask)

dualBit :: Int -> Int -> Lens' Word8 (Bool, Bool)
dualBit upper lower = lens getter setter
  where
    getter w = (w ^. bitwiseValue (bit upper), w ^. bitwiseValue (bit lower))
    setter w (u, l) = w & bitwiseValue (bit upper) .~ u & bitwiseValue (bit lower) .~ l

mkWord16 ::
  -- | HI
  Word8 ->
  -- | LO
  Word8 ->
  Word16
mkWord16 hi lo = ((fromIntegral hi :: Word16) `shiftL` 8) + fromIntegral lo

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = (fromIntegral ((w .&. 0xFF00) `shiftR` 8), fromIntegral (w .&. 0x00FF))