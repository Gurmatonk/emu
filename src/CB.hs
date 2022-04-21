{-# LANGUAGE RankNTypes #-}

module CB where

import Control.Lens hiding (set)
import Data.Bits (bit, rotateL, rotateR)
import Data.Word (Word8)
import Data.Tuple (swap)
import Gameboy
import Utils (bitwiseValue)

cb :: Gameboy -> Gameboy
cb = uncurry execcb . swap . pcLookup

execcb :: Word8 -> Gameboy -> Gameboy
execcb opcode =
  case opcode of
    0x00 -> rlcB
    0x01 -> rlcC
    0x02 -> rlcD
    0x03 -> rlcE
    0x04 -> rlcH
    0x05 -> rlcL
    0x06 -> rlcHL
    0x07 -> rlcA
    0x08 -> rrcB
    0x09 -> rrcC
    0x0A -> rrcD
    0x0B -> rrcE
    0x0C -> rrcH
    0x0D -> rrcL
    0x0E -> rrcHL
    0x0F -> rrcA
    0x80 -> res0B
    0x81 -> res0C
    0x82 -> res0D
    0x83 -> res0E
    0x84 -> res0H
    0x85 -> res0L
    0x86 -> res0HL
    0x87 -> res0A
    0x88 -> res1B
    0x89 -> res1C
    0x8A -> res1D
    0x8B -> res1E
    0x8C -> res1H
    0x8D -> res1L
    0x8E -> res1HL
    0x8F -> res1A
    0x90 -> res2B
    0x91 -> res2C
    0x92 -> res2D
    0x93 -> res2E
    0x94 -> res2H
    0x95 -> res2L
    0x96 -> res2HL
    0x97 -> res2A
    0x98 -> res3B
    0x99 -> res3C
    0x9A -> res3D
    0x9B -> res3E
    0x9C -> res3H
    0x9D -> res3L
    0x9E -> res3HL
    0x9F -> res3A
    0xA0 -> res4B
    0xA1 -> res4C
    0xA2 -> res4D
    0xA3 -> res4E
    0xA4 -> res4H
    0xA5 -> res4L
    0xA6 -> res4HL
    0xA7 -> res4A
    0xA8 -> res5B
    0xA9 -> res5C
    0xAA -> res5D
    0xAB -> res5E
    0xAC -> res5H
    0xAD -> res5L
    0xAE -> res5HL
    0xAF -> res5A
    0xB0 -> res6B
    0xB1 -> res6C
    0xB2 -> res6D
    0xB3 -> res6E
    0xB4 -> res6H
    0xB5 -> res6L
    0xB6 -> res6HL
    0xB7 -> res6A
    0xB8 -> res7B
    0xB9 -> res7C
    0xBA -> res7D
    0xBB -> res7E
    0xBC -> res7H
    0xBD -> res7L
    0xBE -> res7HL
    0xBF -> res7A
    0xC0 -> set0B
    0xC1 -> set0C
    0xC2 -> set0D
    0xC3 -> set0E
    0xC4 -> set0H
    0xC5 -> set0L
    0xC6 -> set0HL
    0xC7 -> set0A
    0xC8 -> set1B
    0xC9 -> set1C
    0xCA -> set1D
    0xCB -> set1E
    0xCC -> set1H
    0xCD -> set1L
    0xCE -> set1HL
    0xCF -> set1A
    0xD0 -> set2B
    0xD1 -> set2C
    0xD2 -> set2D
    0xD3 -> set2E
    0xD4 -> set2H
    0xD5 -> set2L
    0xD6 -> set2HL
    0xD7 -> set2A
    0xD8 -> set3B
    0xD9 -> set3C
    0xDA -> set3D
    0xDB -> set3E
    0xDC -> set3H
    0xDD -> set3L
    0xDE -> set3HL
    0xDF -> set3A
    0xE0 -> set4B
    0xE1 -> set4C
    0xE2 -> set4D
    0xE3 -> set4E
    0xE4 -> set4H
    0xE5 -> set4L
    0xE6 -> set4HL
    0xE7 -> set4A
    0xE8 -> set5B
    0xE9 -> set5C
    0xEA -> set5D
    0xEB -> set5E
    0xEC -> set5H
    0xED -> set5L
    0xEE -> set5HL
    0xEF -> set5A
    0xF0 -> set6B
    0xF1 -> set6C
    0xF2 -> set6D
    0xF3 -> set6E
    0xF4 -> set6H
    0xF5 -> set6L
    0xF6 -> set6HL
    0xF7 -> set6A
    0xF8 -> set7B
    0xF9 -> set7C
    0xFA -> set7D
    0xFB -> set7E
    0xFC -> set7H
    0xFD -> set7L
    0xFE -> set7HL
    0xFF -> set7A
    _ -> undefined

-- NOTE: From what I currently understand, RxCA operations only SET the carry to the bit carried over, but DO NOT rotate through it,
--       i.e. the old carry value is overwritten and gone
rlc :: Lens' CPU Word8 -> Gameboy -> Gameboy
rlc reg gb =
  gb & gbCPU . cpuFlagC .~ old7th
    & gbCPU . reg %~ (`rotateL` 1)
    & (\gb' -> gb' & gbCPU . cpuFlagZ .~ (gb' ^. gbCPU . reg == 0x00))
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    old7th = gb ^. gbCPU . reg . bitwiseValue (bit 7)

rlcB :: Gameboy -> Gameboy
rlcB = rlc cpuRegisterB

rlcC :: Gameboy -> Gameboy
rlcC = rlc cpuRegisterC

rlcD :: Gameboy -> Gameboy
rlcD = rlc cpuRegisterD

rlcE :: Gameboy -> Gameboy
rlcE = rlc cpuRegisterE

rlcH :: Gameboy -> Gameboy
rlcH = rlc cpuRegisterH

rlcL :: Gameboy -> Gameboy
rlcL = rlc cpuRegisterL

rlcHL :: Gameboy -> Gameboy
rlcHL gb =
  gb & gbCPU . cpuFlagC .~ old7th
    & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
    & gbCPU . cpuFlagZ .~ (newVal == 0x00)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    newVal = oldVal `rotateL` 1
    oldVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb
    old7th = oldVal ^. bitwiseValue (bit 7)

rlcA :: Gameboy -> Gameboy
rlcA = rlc cpuRegisterA

rrc :: Lens' CPU Word8 -> Gameboy -> Gameboy
rrc reg gb =
  gb & gbCPU . cpuFlagC .~ old0th
    & gbCPU . reg %~ (`rotateR` 1)
    & (\gb' -> gb' & gbCPU . cpuFlagZ .~ (gb' ^. gbCPU . reg == 0x00))
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    old0th = gb ^. gbCPU . reg . bitwiseValue (bit 0)

rrcB :: Gameboy -> Gameboy
rrcB = rrc cpuRegisterB

rrcC :: Gameboy -> Gameboy
rrcC = rrc cpuRegisterC

rrcD :: Gameboy -> Gameboy
rrcD = rrc cpuRegisterD

rrcE :: Gameboy -> Gameboy
rrcE = rrc cpuRegisterE

rrcH :: Gameboy -> Gameboy
rrcH = rrc cpuRegisterH

rrcL :: Gameboy -> Gameboy
rrcL = rrc cpuRegisterL

rrcHL :: Gameboy -> Gameboy
rrcHL gb =
  gb & gbCPU . cpuFlagC .~ old0th
    & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
    & gbCPU . cpuFlagZ .~ (newVal == 0x00)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    newVal = oldVal `rotateR` 1
    oldVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb
    old0th = oldVal ^. bitwiseValue (bit 0)

rrcA :: Gameboy -> Gameboy
rrcA = rrc cpuRegisterA

res :: Int -> Lens' CPU Word8 -> Gameboy -> Gameboy
res b reg = gbCPU . reg . bitwiseValue (bit b) .~ False

res0B :: Gameboy -> Gameboy
res0B = res 0 cpuRegisterB

res1B :: Gameboy -> Gameboy
res1B = res 1 cpuRegisterB

res2B :: Gameboy -> Gameboy
res2B = res 2 cpuRegisterB

res3B :: Gameboy -> Gameboy
res3B = res 3 cpuRegisterB

res4B :: Gameboy -> Gameboy
res4B = res 4 cpuRegisterB

res5B :: Gameboy -> Gameboy
res5B = res 5 cpuRegisterB

res6B :: Gameboy -> Gameboy
res6B = res 6 cpuRegisterB

res7B :: Gameboy -> Gameboy
res7B = res 7 cpuRegisterB

res0C :: Gameboy -> Gameboy
res0C = res 0 cpuRegisterC

res1C :: Gameboy -> Gameboy
res1C = res 1 cpuRegisterC

res2C :: Gameboy -> Gameboy
res2C = res 2 cpuRegisterC

res3C :: Gameboy -> Gameboy
res3C = res 3 cpuRegisterC

res4C :: Gameboy -> Gameboy
res4C = res 4 cpuRegisterC

res5C :: Gameboy -> Gameboy
res5C = res 5 cpuRegisterC

res6C :: Gameboy -> Gameboy
res6C = res 6 cpuRegisterC

res7C :: Gameboy -> Gameboy
res7C = res 7 cpuRegisterC

res0D :: Gameboy -> Gameboy
res0D = res 0 cpuRegisterD

res1D :: Gameboy -> Gameboy
res1D = res 1 cpuRegisterD

res2D :: Gameboy -> Gameboy
res2D = res 2 cpuRegisterD

res3D :: Gameboy -> Gameboy
res3D = res 3 cpuRegisterD

res4D :: Gameboy -> Gameboy
res4D = res 4 cpuRegisterD

res5D :: Gameboy -> Gameboy
res5D = res 5 cpuRegisterD

res6D :: Gameboy -> Gameboy
res6D = res 6 cpuRegisterD

res7D :: Gameboy -> Gameboy
res7D = res 7 cpuRegisterD

res0E :: Gameboy -> Gameboy
res0E = res 0 cpuRegisterE

res1E :: Gameboy -> Gameboy
res1E = res 1 cpuRegisterE

res2E :: Gameboy -> Gameboy
res2E = res 2 cpuRegisterE

res3E :: Gameboy -> Gameboy
res3E = res 3 cpuRegisterE

res4E :: Gameboy -> Gameboy
res4E = res 4 cpuRegisterE

res5E :: Gameboy -> Gameboy
res5E = res 5 cpuRegisterE

res6E :: Gameboy -> Gameboy
res6E = res 6 cpuRegisterE

res7E :: Gameboy -> Gameboy
res7E = res 7 cpuRegisterE

res0H :: Gameboy -> Gameboy
res0H = res 0 cpuRegisterH

res1H :: Gameboy -> Gameboy
res1H = res 1 cpuRegisterH

res2H :: Gameboy -> Gameboy
res2H = res 2 cpuRegisterH

res3H :: Gameboy -> Gameboy
res3H = res 3 cpuRegisterH

res4H :: Gameboy -> Gameboy
res4H = res 4 cpuRegisterH

res5H :: Gameboy -> Gameboy
res5H = res 5 cpuRegisterH

res6H :: Gameboy -> Gameboy
res6H = res 6 cpuRegisterH

res7H :: Gameboy -> Gameboy
res7H = res 7 cpuRegisterH

res0L :: Gameboy -> Gameboy
res0L = res 0 cpuRegisterL

res1L :: Gameboy -> Gameboy
res1L = res 1 cpuRegisterL

res2L :: Gameboy -> Gameboy
res2L = res 2 cpuRegisterL

res3L :: Gameboy -> Gameboy
res3L = res 3 cpuRegisterL

res4L :: Gameboy -> Gameboy
res4L = res 4 cpuRegisterL

res5L :: Gameboy -> Gameboy
res5L = res 5 cpuRegisterL

res6L :: Gameboy -> Gameboy
res6L = res 6 cpuRegisterL

res7L :: Gameboy -> Gameboy
res7L = res 7 cpuRegisterL

res0A :: Gameboy -> Gameboy
res0A = res 0 cpuRegisterA

res1A :: Gameboy -> Gameboy
res1A = res 1 cpuRegisterA

res2A :: Gameboy -> Gameboy
res2A = res 2 cpuRegisterA

res3A :: Gameboy -> Gameboy
res3A = res 3 cpuRegisterA

res4A :: Gameboy -> Gameboy
res4A = res 4 cpuRegisterA

res5A :: Gameboy -> Gameboy
res5A = res 5 cpuRegisterA

res6A :: Gameboy -> Gameboy
res6A = res 6 cpuRegisterA

res7A :: Gameboy -> Gameboy
res7A = res 7 cpuRegisterA

res0HL :: Gameboy -> Gameboy
res0HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 0) .~ False

res1HL :: Gameboy -> Gameboy
res1HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 1) .~ False

res2HL :: Gameboy -> Gameboy
res2HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 2) .~ False

res3HL :: Gameboy -> Gameboy
res3HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 3) .~ False

res4HL :: Gameboy -> Gameboy
res4HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 4) .~ False

res5HL :: Gameboy -> Gameboy
res5HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 5) .~ False

res6HL :: Gameboy -> Gameboy
res6HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 6) .~ False

res7HL :: Gameboy -> Gameboy
res7HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 7) .~ False

set :: Int -> Lens' CPU Word8 -> Gameboy -> Gameboy
set b reg = gbCPU . reg . bitwiseValue (bit b) .~ True

set0B :: Gameboy -> Gameboy
set0B = set 0 cpuRegisterB

set1B :: Gameboy -> Gameboy
set1B = set 1 cpuRegisterB

set2B :: Gameboy -> Gameboy
set2B = set 2 cpuRegisterB

set3B :: Gameboy -> Gameboy
set3B = set 3 cpuRegisterB

set4B :: Gameboy -> Gameboy
set4B = set 4 cpuRegisterB

set5B :: Gameboy -> Gameboy
set5B = set 5 cpuRegisterB

set6B :: Gameboy -> Gameboy
set6B = set 6 cpuRegisterB

set7B :: Gameboy -> Gameboy
set7B = set 7 cpuRegisterB

set0C :: Gameboy -> Gameboy
set0C = set 0 cpuRegisterC

set1C :: Gameboy -> Gameboy
set1C = set 1 cpuRegisterC

set2C :: Gameboy -> Gameboy
set2C = set 2 cpuRegisterC

set3C :: Gameboy -> Gameboy
set3C = set 3 cpuRegisterC

set4C :: Gameboy -> Gameboy
set4C = set 4 cpuRegisterC

set5C :: Gameboy -> Gameboy
set5C = set 5 cpuRegisterC

set6C :: Gameboy -> Gameboy
set6C = set 6 cpuRegisterC

set7C :: Gameboy -> Gameboy
set7C = set 7 cpuRegisterC

set0D :: Gameboy -> Gameboy
set0D = set 0 cpuRegisterD

set1D :: Gameboy -> Gameboy
set1D = set 1 cpuRegisterD

set2D :: Gameboy -> Gameboy
set2D = set 2 cpuRegisterD

set3D :: Gameboy -> Gameboy
set3D = set 3 cpuRegisterD

set4D :: Gameboy -> Gameboy
set4D = set 4 cpuRegisterD

set5D :: Gameboy -> Gameboy
set5D = set 5 cpuRegisterD

set6D :: Gameboy -> Gameboy
set6D = set 6 cpuRegisterD

set7D :: Gameboy -> Gameboy
set7D = set 7 cpuRegisterD

set0E :: Gameboy -> Gameboy
set0E = set 0 cpuRegisterE

set1E :: Gameboy -> Gameboy
set1E = set 1 cpuRegisterE

set2E :: Gameboy -> Gameboy
set2E = set 2 cpuRegisterE

set3E :: Gameboy -> Gameboy
set3E = set 3 cpuRegisterE

set4E :: Gameboy -> Gameboy
set4E = set 4 cpuRegisterE

set5E :: Gameboy -> Gameboy
set5E = set 5 cpuRegisterE

set6E :: Gameboy -> Gameboy
set6E = set 6 cpuRegisterE

set7E :: Gameboy -> Gameboy
set7E = set 7 cpuRegisterE

set0H :: Gameboy -> Gameboy
set0H = set 0 cpuRegisterH

set1H :: Gameboy -> Gameboy
set1H = set 1 cpuRegisterH

set2H :: Gameboy -> Gameboy
set2H = set 2 cpuRegisterH

set3H :: Gameboy -> Gameboy
set3H = set 3 cpuRegisterH

set4H :: Gameboy -> Gameboy
set4H = set 4 cpuRegisterH

set5H :: Gameboy -> Gameboy
set5H = set 5 cpuRegisterH

set6H :: Gameboy -> Gameboy
set6H = set 6 cpuRegisterH

set7H :: Gameboy -> Gameboy
set7H = set 7 cpuRegisterH

set0L :: Gameboy -> Gameboy
set0L = set 0 cpuRegisterL

set1L :: Gameboy -> Gameboy
set1L = set 1 cpuRegisterL

set2L :: Gameboy -> Gameboy
set2L = set 2 cpuRegisterL

set3L :: Gameboy -> Gameboy
set3L = set 3 cpuRegisterL

set4L :: Gameboy -> Gameboy
set4L = set 4 cpuRegisterL

set5L :: Gameboy -> Gameboy
set5L = set 5 cpuRegisterL

set6L :: Gameboy -> Gameboy
set6L = set 6 cpuRegisterL

set7L :: Gameboy -> Gameboy
set7L = set 7 cpuRegisterL

set0A :: Gameboy -> Gameboy
set0A = set 0 cpuRegisterA

set1A :: Gameboy -> Gameboy
set1A = set 1 cpuRegisterA

set2A :: Gameboy -> Gameboy
set2A = set 2 cpuRegisterA

set3A :: Gameboy -> Gameboy
set3A = set 3 cpuRegisterA

set4A :: Gameboy -> Gameboy
set4A = set 4 cpuRegisterA

set5A :: Gameboy -> Gameboy
set5A = set 5 cpuRegisterA

set6A :: Gameboy -> Gameboy
set6A = set 6 cpuRegisterA

set7A :: Gameboy -> Gameboy
set7A = set 7 cpuRegisterA

set0HL :: Gameboy -> Gameboy
set0HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 0) .~ True

set1HL :: Gameboy -> Gameboy
set1HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 1) .~ True

set2HL :: Gameboy -> Gameboy
set2HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 2) .~ True

set3HL :: Gameboy -> Gameboy
set3HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 3) .~ True

set4HL :: Gameboy -> Gameboy
set4HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 4) .~ True

set5HL :: Gameboy -> Gameboy
set5HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 5) .~ True

set6HL :: Gameboy -> Gameboy
set6HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 6) .~ True

set7HL :: Gameboy -> Gameboy
set7HL gb = gb & mcuWrite (gb ^. gbCPU . cpuRegisterHL) newVal
  where
    newVal = mcuLookup (gb ^. gbCPU . cpuRegisterHL) gb & bitwiseValue (bit 7) .~ True