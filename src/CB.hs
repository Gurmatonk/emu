module CB where

import Control.Lens
import Data.Bits (bit)
import Data.Word (Word8)
import Data.Tuple (swap)
import Gameboy

cb :: Gameboy -> Gameboy
cb = uncurry execcb . swap . pcLookup

execcb :: Word8 -> Gameboy -> Gameboy
execcb opcode =
  case opcode of
    0x80 -> res0B
    _ -> undefined

res0B :: Gameboy -> Gameboy
res0B = gbCPU . cpuRegisterB . bitwiseValue (bit 0) .~ False