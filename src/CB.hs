{-# LANGUAGE RankNTypes #-}

module CB where

import Control.Lens hiding (set)
import Data.Bits (bit, rotateL, rotateR)
import Data.Word (Word8)
import Data.Tuple (swap)

import Types
import Lenses (cpuFlagC, cpuFlagH, cpuFlagN, cpuFlagZ, cpuRegisterHL, mcuLookup, mcuWrite, pcLookup)
import Utils (bitwiseValue)

cb :: CPU -> CPU
cb = uncurry execcb . swap . pcLookup

execcb :: Word8 -> CPU -> CPU
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
rlc :: Lens' CPU Word8 -> CPU -> CPU
rlc reg cpu =
  cpu & cpuFlagC .~ old7th
    & reg %~ (`rotateL` 1)
    & (\cpu' -> cpu' & cpuFlagZ .~ (cpu' ^. reg == 0x00))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old7th = cpu ^. reg . bitwiseValue (bit 7)

rlcB :: CPU -> CPU
rlcB = rlc cpuRegisterB

rlcC :: CPU -> CPU
rlcC = rlc cpuRegisterC

rlcD :: CPU -> CPU
rlcD = rlc cpuRegisterD

rlcE :: CPU -> CPU
rlcE = rlc cpuRegisterE

rlcH :: CPU -> CPU
rlcH = rlc cpuRegisterH

rlcL :: CPU -> CPU
rlcL = rlc cpuRegisterL

rlcHL :: CPU -> CPU
rlcHL cpu =
  cpu & cpuFlagC .~ old7th
    & mcuWrite (cpu ^. cpuRegisterHL) newVal
    & cpuFlagZ .~ (newVal == 0x00)
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = oldVal `rotateL` 1
    oldVal = mcuLookup (cpu ^. cpuRegisterHL) cpu
    old7th = oldVal ^. bitwiseValue (bit 7)

rlcA :: CPU -> CPU
rlcA = rlc cpuRegisterA

rrc :: Lens' CPU Word8 -> CPU -> CPU
rrc reg cpu =
  cpu & cpuFlagC .~ old0th
    & reg %~ (`rotateR` 1)
    & (\cpu' -> cpu' & cpuFlagZ .~ (cpu' ^. reg == 0x00))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old0th = cpu ^. reg . bitwiseValue (bit 0)

rrcB :: CPU -> CPU
rrcB = rrc cpuRegisterB

rrcC :: CPU -> CPU
rrcC = rrc cpuRegisterC

rrcD :: CPU -> CPU
rrcD = rrc cpuRegisterD

rrcE :: CPU -> CPU
rrcE = rrc cpuRegisterE

rrcH :: CPU -> CPU
rrcH = rrc cpuRegisterH

rrcL :: CPU -> CPU
rrcL = rrc cpuRegisterL

rrcHL :: CPU -> CPU
rrcHL cpu =
  cpu & cpuFlagC .~ old0th
    & mcuWrite (cpu ^. cpuRegisterHL) newVal
    & cpuFlagZ .~ (newVal == 0x00)
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = oldVal `rotateR` 1
    oldVal = mcuLookup (cpu ^. cpuRegisterHL) cpu
    old0th = oldVal ^. bitwiseValue (bit 0)

rrcA :: CPU -> CPU
rrcA = rrc cpuRegisterA

res :: Int -> Lens' CPU Word8 -> CPU -> CPU
res b reg = reg . bitwiseValue (bit b) .~ False

res0B :: CPU -> CPU
res0B = res 0 cpuRegisterB

res1B :: CPU -> CPU
res1B = res 1 cpuRegisterB

res2B :: CPU -> CPU
res2B = res 2 cpuRegisterB

res3B :: CPU -> CPU
res3B = res 3 cpuRegisterB

res4B :: CPU -> CPU
res4B = res 4 cpuRegisterB

res5B :: CPU -> CPU
res5B = res 5 cpuRegisterB

res6B :: CPU -> CPU
res6B = res 6 cpuRegisterB

res7B :: CPU -> CPU
res7B = res 7 cpuRegisterB

res0C :: CPU -> CPU
res0C = res 0 cpuRegisterC

res1C :: CPU -> CPU
res1C = res 1 cpuRegisterC

res2C :: CPU -> CPU
res2C = res 2 cpuRegisterC

res3C :: CPU -> CPU
res3C = res 3 cpuRegisterC

res4C :: CPU -> CPU
res4C = res 4 cpuRegisterC

res5C :: CPU -> CPU
res5C = res 5 cpuRegisterC

res6C :: CPU -> CPU
res6C = res 6 cpuRegisterC

res7C :: CPU -> CPU
res7C = res 7 cpuRegisterC

res0D :: CPU -> CPU
res0D = res 0 cpuRegisterD

res1D :: CPU -> CPU
res1D = res 1 cpuRegisterD

res2D :: CPU -> CPU
res2D = res 2 cpuRegisterD

res3D :: CPU -> CPU
res3D = res 3 cpuRegisterD

res4D :: CPU -> CPU
res4D = res 4 cpuRegisterD

res5D :: CPU -> CPU
res5D = res 5 cpuRegisterD

res6D :: CPU -> CPU
res6D = res 6 cpuRegisterD

res7D :: CPU -> CPU
res7D = res 7 cpuRegisterD

res0E :: CPU -> CPU
res0E = res 0 cpuRegisterE

res1E :: CPU -> CPU
res1E = res 1 cpuRegisterE

res2E :: CPU -> CPU
res2E = res 2 cpuRegisterE

res3E :: CPU -> CPU
res3E = res 3 cpuRegisterE

res4E :: CPU -> CPU
res4E = res 4 cpuRegisterE

res5E :: CPU -> CPU
res5E = res 5 cpuRegisterE

res6E :: CPU -> CPU
res6E = res 6 cpuRegisterE

res7E :: CPU -> CPU
res7E = res 7 cpuRegisterE

res0H :: CPU -> CPU
res0H = res 0 cpuRegisterH

res1H :: CPU -> CPU
res1H = res 1 cpuRegisterH

res2H :: CPU -> CPU
res2H = res 2 cpuRegisterH

res3H :: CPU -> CPU
res3H = res 3 cpuRegisterH

res4H :: CPU -> CPU
res4H = res 4 cpuRegisterH

res5H :: CPU -> CPU
res5H = res 5 cpuRegisterH

res6H :: CPU -> CPU
res6H = res 6 cpuRegisterH

res7H :: CPU -> CPU
res7H = res 7 cpuRegisterH

res0L :: CPU -> CPU
res0L = res 0 cpuRegisterL

res1L :: CPU -> CPU
res1L = res 1 cpuRegisterL

res2L :: CPU -> CPU
res2L = res 2 cpuRegisterL

res3L :: CPU -> CPU
res3L = res 3 cpuRegisterL

res4L :: CPU -> CPU
res4L = res 4 cpuRegisterL

res5L :: CPU -> CPU
res5L = res 5 cpuRegisterL

res6L :: CPU -> CPU
res6L = res 6 cpuRegisterL

res7L :: CPU -> CPU
res7L = res 7 cpuRegisterL

res0A :: CPU -> CPU
res0A = res 0 cpuRegisterA

res1A :: CPU -> CPU
res1A = res 1 cpuRegisterA

res2A :: CPU -> CPU
res2A = res 2 cpuRegisterA

res3A :: CPU -> CPU
res3A = res 3 cpuRegisterA

res4A :: CPU -> CPU
res4A = res 4 cpuRegisterA

res5A :: CPU -> CPU
res5A = res 5 cpuRegisterA

res6A :: CPU -> CPU
res6A = res 6 cpuRegisterA

res7A :: CPU -> CPU
res7A = res 7 cpuRegisterA

res0HL :: CPU -> CPU
res0HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 0) .~ False

res1HL :: CPU -> CPU
res1HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 1) .~ False

res2HL :: CPU -> CPU
res2HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 2) .~ False

res3HL :: CPU -> CPU
res3HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 3) .~ False

res4HL :: CPU -> CPU
res4HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 4) .~ False

res5HL :: CPU -> CPU
res5HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 5) .~ False

res6HL :: CPU -> CPU
res6HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 6) .~ False

res7HL :: CPU -> CPU
res7HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 7) .~ False

set :: Int -> Lens' CPU Word8 -> CPU -> CPU
set b reg = reg . bitwiseValue (bit b) .~ True

set0B :: CPU -> CPU
set0B = set 0 cpuRegisterB

set1B :: CPU -> CPU
set1B = set 1 cpuRegisterB

set2B :: CPU -> CPU
set2B = set 2 cpuRegisterB

set3B :: CPU -> CPU
set3B = set 3 cpuRegisterB

set4B :: CPU -> CPU
set4B = set 4 cpuRegisterB

set5B :: CPU -> CPU
set5B = set 5 cpuRegisterB

set6B :: CPU -> CPU
set6B = set 6 cpuRegisterB

set7B :: CPU -> CPU
set7B = set 7 cpuRegisterB

set0C :: CPU -> CPU
set0C = set 0 cpuRegisterC

set1C :: CPU -> CPU
set1C = set 1 cpuRegisterC

set2C :: CPU -> CPU
set2C = set 2 cpuRegisterC

set3C :: CPU -> CPU
set3C = set 3 cpuRegisterC

set4C :: CPU -> CPU
set4C = set 4 cpuRegisterC

set5C :: CPU -> CPU
set5C = set 5 cpuRegisterC

set6C :: CPU -> CPU
set6C = set 6 cpuRegisterC

set7C :: CPU -> CPU
set7C = set 7 cpuRegisterC

set0D :: CPU -> CPU
set0D = set 0 cpuRegisterD

set1D :: CPU -> CPU
set1D = set 1 cpuRegisterD

set2D :: CPU -> CPU
set2D = set 2 cpuRegisterD

set3D :: CPU -> CPU
set3D = set 3 cpuRegisterD

set4D :: CPU -> CPU
set4D = set 4 cpuRegisterD

set5D :: CPU -> CPU
set5D = set 5 cpuRegisterD

set6D :: CPU -> CPU
set6D = set 6 cpuRegisterD

set7D :: CPU -> CPU
set7D = set 7 cpuRegisterD

set0E :: CPU -> CPU
set0E = set 0 cpuRegisterE

set1E :: CPU -> CPU
set1E = set 1 cpuRegisterE

set2E :: CPU -> CPU
set2E = set 2 cpuRegisterE

set3E :: CPU -> CPU
set3E = set 3 cpuRegisterE

set4E :: CPU -> CPU
set4E = set 4 cpuRegisterE

set5E :: CPU -> CPU
set5E = set 5 cpuRegisterE

set6E :: CPU -> CPU
set6E = set 6 cpuRegisterE

set7E :: CPU -> CPU
set7E = set 7 cpuRegisterE

set0H :: CPU -> CPU
set0H = set 0 cpuRegisterH

set1H :: CPU -> CPU
set1H = set 1 cpuRegisterH

set2H :: CPU -> CPU
set2H = set 2 cpuRegisterH

set3H :: CPU -> CPU
set3H = set 3 cpuRegisterH

set4H :: CPU -> CPU
set4H = set 4 cpuRegisterH

set5H :: CPU -> CPU
set5H = set 5 cpuRegisterH

set6H :: CPU -> CPU
set6H = set 6 cpuRegisterH

set7H :: CPU -> CPU
set7H = set 7 cpuRegisterH

set0L :: CPU -> CPU
set0L = set 0 cpuRegisterL

set1L :: CPU -> CPU
set1L = set 1 cpuRegisterL

set2L :: CPU -> CPU
set2L = set 2 cpuRegisterL

set3L :: CPU -> CPU
set3L = set 3 cpuRegisterL

set4L :: CPU -> CPU
set4L = set 4 cpuRegisterL

set5L :: CPU -> CPU
set5L = set 5 cpuRegisterL

set6L :: CPU -> CPU
set6L = set 6 cpuRegisterL

set7L :: CPU -> CPU
set7L = set 7 cpuRegisterL

set0A :: CPU -> CPU
set0A = set 0 cpuRegisterA

set1A :: CPU -> CPU
set1A = set 1 cpuRegisterA

set2A :: CPU -> CPU
set2A = set 2 cpuRegisterA

set3A :: CPU -> CPU
set3A = set 3 cpuRegisterA

set4A :: CPU -> CPU
set4A = set 4 cpuRegisterA

set5A :: CPU -> CPU
set5A = set 5 cpuRegisterA

set6A :: CPU -> CPU
set6A = set 6 cpuRegisterA

set7A :: CPU -> CPU
set7A = set 7 cpuRegisterA

set0HL :: CPU -> CPU
set0HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 0) .~ True

set1HL :: CPU -> CPU
set1HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 1) .~ True

set2HL :: CPU -> CPU
set2HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 2) .~ True

set3HL :: CPU -> CPU
set3HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 3) .~ True

set4HL :: CPU -> CPU
set4HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 4) .~ True

set5HL :: CPU -> CPU
set5HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 5) .~ True

set6HL :: CPU -> CPU
set6HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 6) .~ True

set7HL :: CPU -> CPU
set7HL cpu = cpu & mcuWrite (cpu ^. cpuRegisterHL) newVal
  where
    newVal = mcuLookup (cpu ^. cpuRegisterHL) cpu & bitwiseValue (bit 7) .~ True