{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module CB where

import Control.Lens hiding (set)
import Data.Bits (bit, rotateL, rotateR, shiftL, shiftR)
import qualified Data.Tuple as T
import Data.Word (Word8)
import Lenses (cpuFlagC, cpuFlagH, cpuFlagN, cpuRegisterHL, mcuLookup, mcuWrite, pcLookup, setCpuFlagZ)
import Types
import Utils (swapWord8, setBitF, clearBitF, testBitF, bitTo)

cb :: CPU -> (CPU, Cycles)
cb = uncurry execcb . T.swap . pcLookup

execcb :: Word8 -> CPU -> (CPU, Cycles)
execcb opcode =
  case opcode of
    0x00 -> (,8) . rlc cpuRegisterB
    0x01 -> (,8) . rlc cpuRegisterC
    0x02 -> (,8) . rlc cpuRegisterD
    0x03 -> (,8) . rlc cpuRegisterE
    0x04 -> (,8) . rlc cpuRegisterH
    0x05 -> (,8) . rlc cpuRegisterL
    0x06 -> (,16) . rlcHL
    0x07 -> (,8) . rlc cpuRegisterA
    0x08 -> (,8) . rrc cpuRegisterB
    0x09 -> (,8) . rrc cpuRegisterC
    0x0A -> (,8) . rrc cpuRegisterD
    0x0B -> (,8) . rrc cpuRegisterE
    0x0C -> (,8) . rrc cpuRegisterH
    0x0D -> (,8) . rrc cpuRegisterL
    0x0E -> (,16) . rrcHL
    0x0F -> (,8) . rrc cpuRegisterA
    0x10 -> (,8) . rl cpuRegisterB
    0x11 -> (,8) . rl cpuRegisterC
    0x12 -> (,8) . rl cpuRegisterD
    0x13 -> (,8) . rl cpuRegisterE
    0x14 -> (,8) . rl cpuRegisterH
    0x15 -> (,8) . rl cpuRegisterL
    0x16 -> (,16) . rlHL
    0x17 -> (,8) . rl cpuRegisterA
    0x18 -> (,8) . rr cpuRegisterB
    0x19 -> (,8) . rr cpuRegisterC
    0x1A -> (,8) . rr cpuRegisterD
    0x1B -> (,8) . rr cpuRegisterE
    0x1C -> (,8) . rr cpuRegisterH
    0x1D -> (,8) . rr cpuRegisterL
    0x1E -> (,16) . rrHL
    0x1F -> (,8) . rr cpuRegisterA
    0x20 -> (,8) . sla cpuRegisterB
    0x21 -> (,8) . sla cpuRegisterC
    0x22 -> (,8) . sla cpuRegisterD
    0x23 -> (,8) . sla cpuRegisterE
    0x24 -> (,8) . sla cpuRegisterH
    0x25 -> (,8) . sla cpuRegisterL
    0x26 -> (,16) . slaHL
    0x27 -> (,8) . sla cpuRegisterA
    0x28 -> (,8) . sra cpuRegisterB
    0x29 -> (,8) . sra cpuRegisterC
    0x2A -> (,8) . sra cpuRegisterD
    0x2B -> (,8) . sra cpuRegisterE
    0x2C -> (,8) . sra cpuRegisterH
    0x2D -> (,8) . sra cpuRegisterL
    0x2E -> (,16) . sraHL
    0x2F -> (,8) . sra cpuRegisterA
    0x30 -> (,8) . swap cpuRegisterB
    0x31 -> (,8) . swap cpuRegisterC
    0x32 -> (,8) . swap cpuRegisterD
    0x33 -> (,8) . swap cpuRegisterE
    0x34 -> (,8) . swap cpuRegisterH
    0x35 -> (,8) . swap cpuRegisterL
    0x36 -> (,16) . swapHL
    0x37 -> (,8) . swap cpuRegisterA
    0x38 -> (,8) . srl cpuRegisterB
    0x39 -> (,8) . srl cpuRegisterC
    0x3A -> (,8) . srl cpuRegisterD
    0x3B -> (,8) . srl cpuRegisterE
    0x3C -> (,8) . srl cpuRegisterH
    0x3D -> (,8) . srl cpuRegisterL
    0x3E -> (,16) . srlHL
    0x3F -> (,8) . srl cpuRegisterA
    0x40 -> (,8) . testBit 0 cpuRegisterB
    0x41 -> (,8) . testBit 0 cpuRegisterC
    0x42 -> (,8) . testBit 0 cpuRegisterD
    0x43 -> (,8) . testBit 0 cpuRegisterE
    0x44 -> (,8) . testBit 0 cpuRegisterH
    0x45 -> (,8) . testBit 0 cpuRegisterL
    0x46 -> (,16) . testBitHL 0
    0x47 -> (,8) . testBit 0 cpuRegisterA
    0x48 -> (,8) . testBit 1 cpuRegisterB
    0x49 -> (,8) . testBit 1 cpuRegisterC
    0x4A -> (,8) . testBit 1 cpuRegisterD
    0x4B -> (,8) . testBit 1 cpuRegisterE
    0x4C -> (,8) . testBit 1 cpuRegisterH
    0x4D -> (,8) . testBit 1 cpuRegisterL
    0x4E -> (,16) . testBitHL 1
    0x4F -> (,8) . testBit 1 cpuRegisterA
    0x50 -> (,8) . testBit 2 cpuRegisterB
    0x51 -> (,8) . testBit 2 cpuRegisterC
    0x52 -> (,8) . testBit 2 cpuRegisterD
    0x53 -> (,8) . testBit 2 cpuRegisterE
    0x54 -> (,8) . testBit 2 cpuRegisterH
    0x55 -> (,8) . testBit 2 cpuRegisterL
    0x56 -> (,16) . testBitHL 2
    0x57 -> (,8) . testBit 2 cpuRegisterA
    0x58 -> (,8) . testBit 3 cpuRegisterB
    0x59 -> (,8) . testBit 3 cpuRegisterC
    0x5A -> (,8) . testBit 3 cpuRegisterD
    0x5B -> (,8) . testBit 3 cpuRegisterE
    0x5C -> (,8) . testBit 3 cpuRegisterH
    0x5D -> (,8) . testBit 3 cpuRegisterL
    0x5E -> (,16) . testBitHL 3
    0x5F -> (,8) . testBit 3 cpuRegisterA
    0x60 -> (,8) . testBit 4 cpuRegisterB
    0x61 -> (,8) . testBit 4 cpuRegisterC
    0x62 -> (,8) . testBit 4 cpuRegisterD
    0x63 -> (,8) . testBit 4 cpuRegisterE
    0x64 -> (,8) . testBit 4 cpuRegisterH
    0x65 -> (,8) . testBit 4 cpuRegisterL
    0x66 -> (,16) . testBitHL 4
    0x67 -> (,8) . testBit 4 cpuRegisterA
    0x68 -> (,8) . testBit 5 cpuRegisterB
    0x69 -> (,8) . testBit 5 cpuRegisterC
    0x6A -> (,8) . testBit 5 cpuRegisterD
    0x6B -> (,8) . testBit 5 cpuRegisterE
    0x6C -> (,8) . testBit 5 cpuRegisterH
    0x6D -> (,8) . testBit 5 cpuRegisterL
    0x6E -> (,16) . testBitHL 5
    0x6F -> (,8) . testBit 5 cpuRegisterA
    0x70 -> (,8) . testBit 6 cpuRegisterB
    0x71 -> (,8) . testBit 6 cpuRegisterC
    0x72 -> (,8) . testBit 6 cpuRegisterD
    0x73 -> (,8) . testBit 6 cpuRegisterE
    0x74 -> (,8) . testBit 6 cpuRegisterH
    0x75 -> (,8) . testBit 6 cpuRegisterL
    0x76 -> (,16) . testBitHL 6
    0x77 -> (,8) . testBit 6 cpuRegisterA
    0x78 -> (,8) . testBit 7 cpuRegisterB
    0x79 -> (,8) . testBit 7 cpuRegisterC
    0x7A -> (,8) . testBit 7 cpuRegisterD
    0x7B -> (,8) . testBit 7 cpuRegisterE
    0x7C -> (,8) . testBit 7 cpuRegisterH
    0x7D -> (,8) . testBit 7 cpuRegisterL
    0x7E -> (,16) . testBitHL 7
    0x7F -> (,8) . testBit 7 cpuRegisterA
    0x80 -> (,8) . res 0 cpuRegisterB
    0x81 -> (,8) . res 0 cpuRegisterC
    0x82 -> (,8) . res 0 cpuRegisterD
    0x83 -> (,8) . res 0 cpuRegisterE
    0x84 -> (,8) . res 0 cpuRegisterH
    0x85 -> (,8) . res 0 cpuRegisterL
    0x86 -> (,16) . res0HL
    0x87 -> (,8) . res 0 cpuRegisterA
    0x88 -> (,8) . res 1 cpuRegisterB
    0x89 -> (,8) . res 1 cpuRegisterC
    0x8A -> (,8) . res 1 cpuRegisterD
    0x8B -> (,8) . res 1 cpuRegisterE
    0x8C -> (,8) . res 1 cpuRegisterH
    0x8D -> (,8) . res 1 cpuRegisterL
    0x8E -> (,16) . res1HL
    0x8F -> (,8) . res 1 cpuRegisterA
    0x90 -> (,8) . res 2 cpuRegisterB
    0x91 -> (,8) . res 2 cpuRegisterC
    0x92 -> (,8) . res 2 cpuRegisterD
    0x93 -> (,8) . res 2 cpuRegisterE
    0x94 -> (,8) . res 2 cpuRegisterH
    0x95 -> (,8) . res 2 cpuRegisterL
    0x96 -> (,16) . res2HL
    0x97 -> (,8) . res 2 cpuRegisterA
    0x98 -> (,8) . res 3 cpuRegisterB
    0x99 -> (,8) . res 3 cpuRegisterC
    0x9A -> (,8) . res 3 cpuRegisterD
    0x9B -> (,8) . res 3 cpuRegisterE
    0x9C -> (,8) . res 3 cpuRegisterH
    0x9D -> (,8) . res 3 cpuRegisterL
    0x9E -> (,16) . res3HL
    0x9F -> (,8) . res 3 cpuRegisterA
    0xA0 -> (,8) . res 4 cpuRegisterB
    0xA1 -> (,8) . res 4 cpuRegisterC
    0xA2 -> (,8) . res 4 cpuRegisterD
    0xA3 -> (,8) . res 4 cpuRegisterE
    0xA4 -> (,8) . res 4 cpuRegisterH
    0xA5 -> (,8) . res 4 cpuRegisterL
    0xA6 -> (,16) . res4HL
    0xA7 -> (,8) . res 4 cpuRegisterA
    0xA8 -> (,8) . res 5 cpuRegisterB
    0xA9 -> (,8) . res 5 cpuRegisterC
    0xAA -> (,8) . res 5 cpuRegisterD
    0xAB -> (,8) . res 5 cpuRegisterE
    0xAC -> (,8) . res 5 cpuRegisterH
    0xAD -> (,8) . res 5 cpuRegisterL
    0xAE -> (,16) . res5HL
    0xAF -> (,8) . res 5 cpuRegisterA
    0xB0 -> (,8) . res 6 cpuRegisterB
    0xB1 -> (,8) . res 6 cpuRegisterC
    0xB2 -> (,8) . res 6 cpuRegisterD
    0xB3 -> (,8) . res 6 cpuRegisterE
    0xB4 -> (,8) . res 6 cpuRegisterH
    0xB5 -> (,8) . res 6 cpuRegisterL
    0xB6 -> (,16) . res6HL
    0xB7 -> (,8) . res 6 cpuRegisterA
    0xB8 -> (,8) . res 7 cpuRegisterB
    0xB9 -> (,8) . res 7 cpuRegisterC
    0xBA -> (,8) . res 7 cpuRegisterD
    0xBB -> (,8) . res 7 cpuRegisterE
    0xBC -> (,8) . res 7 cpuRegisterH
    0xBD -> (,8) . res 7 cpuRegisterL
    0xBE -> (,16) . res7HL
    0xBF -> (,8) . res 7 cpuRegisterA
    0xC0 -> (,8) . set 0 cpuRegisterB
    0xC1 -> (,8) . set 0 cpuRegisterC
    0xC2 -> (,8) . set 0 cpuRegisterD
    0xC3 -> (,8) . set 0 cpuRegisterE
    0xC4 -> (,8) . set 0 cpuRegisterH
    0xC5 -> (,8) . set 0 cpuRegisterL
    0xC6 -> (,16) . set0HL
    0xC7 -> (,8) . set 0 cpuRegisterA
    0xC8 -> (,8) . set 1 cpuRegisterB
    0xC9 -> (,8) . set 1 cpuRegisterC
    0xCA -> (,8) . set 1 cpuRegisterD
    0xCB -> (,8) . set 1 cpuRegisterE
    0xCC -> (,8) . set 1 cpuRegisterH
    0xCD -> (,8) . set 1 cpuRegisterL
    0xCE -> (,16) . set1HL
    0xCF -> (,8) . set 1 cpuRegisterA
    0xD0 -> (,8) . set 2 cpuRegisterB
    0xD1 -> (,8) . set 2 cpuRegisterC
    0xD2 -> (,8) . set 2 cpuRegisterD
    0xD3 -> (,8) . set 2 cpuRegisterE
    0xD4 -> (,8) . set 2 cpuRegisterH
    0xD5 -> (,8) . set 2 cpuRegisterL
    0xD6 -> (,16) . set2HL
    0xD7 -> (,8) . set 2 cpuRegisterA
    0xD8 -> (,8) . set 3 cpuRegisterB
    0xD9 -> (,8) . set 3 cpuRegisterC
    0xDA -> (,8) . set 3 cpuRegisterD
    0xDB -> (,8) . set 3 cpuRegisterE
    0xDC -> (,8) . set 3 cpuRegisterH
    0xDD -> (,8) . set 3 cpuRegisterL
    0xDE -> (,16) . set3HL
    0xDF -> (,8) . set 3 cpuRegisterA
    0xE0 -> (,8) . set 4 cpuRegisterB
    0xE1 -> (,8) . set 4 cpuRegisterC
    0xE2 -> (,8) . set 4 cpuRegisterD
    0xE3 -> (,8) . set 4 cpuRegisterE
    0xE4 -> (,8) . set 4 cpuRegisterH
    0xE5 -> (,8) . set 4 cpuRegisterL
    0xE6 -> (,16) . set4HL
    0xE7 -> (,8) . set 4 cpuRegisterA
    0xE8 -> (,8) . set 5 cpuRegisterB
    0xE9 -> (,8) . set 5 cpuRegisterC
    0xEA -> (,8) . set 5 cpuRegisterD
    0xEB -> (,8) . set 5 cpuRegisterE
    0xEC -> (,8) . set 5 cpuRegisterH
    0xED -> (,8) . set 5 cpuRegisterL
    0xEE -> (,16) . set5HL
    0xEF -> (,8) . set 5 cpuRegisterA
    0xF0 -> (,8) . set 6 cpuRegisterB
    0xF1 -> (,8) . set 6 cpuRegisterC
    0xF2 -> (,8) . set 6 cpuRegisterD
    0xF3 -> (,8) . set 6 cpuRegisterE
    0xF4 -> (,8) . set 6 cpuRegisterH
    0xF5 -> (,8) . set 6 cpuRegisterL
    0xF6 -> (,16) . set6HL
    0xF7 -> (,8) . set 6 cpuRegisterA
    0xF8 -> (,8) . set 7 cpuRegisterB
    0xF9 -> (,8) . set 7 cpuRegisterC
    0xFA -> (,8) . set 7 cpuRegisterD
    0xFB -> (,8) . set 7 cpuRegisterE
    0xFC -> (,8) . set 7 cpuRegisterH
    0xFD -> (,8) . set 7 cpuRegisterL
    0xFE -> (,16) . set7HL
    0xFF -> (,8) . set 7 cpuRegisterA
    _ -> undefined

-- NOTE: From what I currently understand, RxCA operations only SET the carry to the bit carried over, but DO NOT rotate through it,
--       i.e. the old carry value is overwritten and gone
rlc :: Lens' CPU Word8 -> CPU -> CPU
rlc reg cpu =
  cpu & cpuFlagC .~ old7th
    & reg %~ (`rotateL` 1)
    & (\cpu' -> cpu' & setCpuFlagZ (cpu' ^. reg == 0x00))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old7th = testBitF 7 (cpu ^. reg)

rl :: Lens' CPU Word8 -> CPU -> CPU
rl reg cpu =
  cpu & reg %~ bitTo 7 new7th
    & cpuFlagC .~ old7th
    & reg %~ (`rotateL` 1)
    & (\c -> c & setCpuFlagZ (c ^. reg == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old7th = testBitF 7 (cpu ^. reg)
    new7th = cpu ^. cpuFlagC

rlHL :: CPU -> CPU
rlHL cpu =
  cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
    & cpuFlagC .~ old7th
    & (\c -> c & setCpuFlagZ (newVal == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = bitTo 7 new7th oldVal
    old7th = testBitF 7 oldVal
    oldVal = cpu ^. mcuLookup cpuRegisterHL
    new7th = cpu ^. cpuFlagC

rlcHL :: CPU -> CPU
rlcHL cpu =
  cpu & cpuFlagC .~ old7th
    & mcuWrite cpuRegisterHL (to . const $ newVal)
    & setCpuFlagZ (newVal == 0x00)
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = oldVal `rotateL` 1
    oldVal = cpu ^. mcuLookup cpuRegisterHL
    old7th = testBitF 7 oldVal

rr :: Lens' CPU Word8 -> CPU -> CPU
rr reg cpu =
  cpu & reg %~ bitTo 0 new0th
    & cpuFlagC .~ old0th
    & reg %~ (`rotateR` 1)
    & (\c -> c & setCpuFlagZ (c ^. reg == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old0th = testBitF 0 (cpu ^. reg)
    new0th = cpu ^. cpuFlagC

rrHL :: CPU -> CPU
rrHL cpu =
  cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
    & cpuFlagC .~ old0th
    & (\c -> c & setCpuFlagZ (newVal == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = bitTo 0 new0th oldVal
    old0th = testBitF 0 oldVal
    oldVal = cpu ^. mcuLookup cpuRegisterHL
    new0th = cpu ^. cpuFlagC

rrc :: Lens' CPU Word8 -> CPU -> CPU
rrc reg cpu =
  cpu & cpuFlagC .~ old0th
    & reg %~ (`rotateR` 1)
    & (\cpu' -> cpu' & setCpuFlagZ (cpu' ^. reg == 0x00))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old0th = testBitF 0 (cpu ^. reg)

rrcHL :: CPU -> CPU
rrcHL cpu =
  cpu & cpuFlagC .~ old0th
    & mcuWrite cpuRegisterHL (to . const $ newVal)
    & setCpuFlagZ (newVal == 0x00)
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = oldVal `rotateR` 1
    oldVal = cpu ^. mcuLookup cpuRegisterHL
    old0th = testBitF 0 oldVal

sla :: Lens' CPU Word8 -> CPU -> CPU
sla reg cpu =
  cpu & cpuFlagC .~ testBitF 7 (cpu ^. reg)
    & reg %~ (`shiftL` 1)
    & (\c -> c & setCpuFlagZ (c ^. reg == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False

slaHL :: CPU -> CPU
slaHL cpu =
  cpu & cpuFlagC .~ testBitF 7 val
    & mcuWrite cpuRegisterHL (to . const $ newVal)
    & (\c -> c & setCpuFlagZ (newVal == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = shiftL val 1
    val = cpu ^. mcuLookup cpuRegisterHL

sra :: Lens' CPU Word8 -> CPU -> CPU
sra reg cpu =
  cpu & cpuFlagC .~ testBitF 0 (cpu ^. reg)
    & reg %~ (`shiftR` 1)
    & (\c -> c & reg %~ bitTo 7 (testBitF 6 (cpu ^. reg)))
    & (\c -> c & setCpuFlagZ (c ^. reg == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False

sraHL :: CPU -> CPU
sraHL cpu =
  cpu & cpuFlagC .~ testBitF 0 val
    & mcuWrite cpuRegisterHL (to . const $ newVal)
    & (\c -> c & setCpuFlagZ (newVal == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = bitTo 7 (testBitF 7 val) $ shiftR val 1
    val = cpu ^. mcuLookup cpuRegisterHL

srl :: Lens' CPU Word8 -> CPU -> CPU
srl reg cpu =
  cpu & cpuFlagC .~ testBitF 0 (cpu ^. reg)
    & reg %~ (`shiftR` 1)
    & (\c -> c & setCpuFlagZ (c ^. reg == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False

srlHL :: CPU -> CPU
srlHL cpu =
  cpu & cpuFlagC .~ testBitF 0 val
    & mcuWrite cpuRegisterHL (to . const $ newVal)
    & (\c -> c & setCpuFlagZ (newVal == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    newVal = shiftR val 1
    val = cpu ^. mcuLookup cpuRegisterHL

swap :: Lens' CPU Word8 -> CPU -> CPU
swap reg cpu =
  cpu & reg %~ swapWord8
    & (\c -> c & setCpuFlagZ (c ^. reg == 0))
    & cpuFlagN .~ False
    & cpuFlagH .~ False
    & cpuFlagC .~ False

swapHL :: CPU -> CPU
swapHL cpu =
  cpu & mcuWrite cpuRegisterHL (to . const $ val)
    & setCpuFlagZ (val == 0)
    & cpuFlagN .~ False
    & cpuFlagH .~ False
    & cpuFlagC .~ False
  where
    val = cpu ^. mcuLookup cpuRegisterHL . to swapWord8

res :: Int -> Lens' CPU Word8 -> CPU -> CPU
res b reg = reg %~ clearBitF b

res0HL :: CPU -> CPU
res0HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 0

res1HL :: CPU -> CPU
res1HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 1

res2HL :: CPU -> CPU
res2HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 2

res3HL :: CPU -> CPU
res3HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 3

res4HL :: CPU -> CPU
res4HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 4

res5HL :: CPU -> CPU
res5HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 5

res6HL :: CPU -> CPU
res6HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 6

res7HL :: CPU -> CPU
res7HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & clearBitF 7

set :: Int -> Lens' CPU Word8 -> CPU -> CPU
set b reg = reg %~ setBitF b

set0HL :: CPU -> CPU
set0HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 0

set1HL :: CPU -> CPU
set1HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 1

set2HL :: CPU -> CPU
set2HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 2

set3HL :: CPU -> CPU
set3HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 3

set4HL :: CPU -> CPU
set4HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 4

set5HL :: CPU -> CPU
set5HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 5

set6HL :: CPU -> CPU
set6HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 6

set7HL :: CPU -> CPU
set7HL cpu = cpu & mcuWrite cpuRegisterHL (to . const $ newVal)
  where
    newVal = cpu ^. mcuLookup cpuRegisterHL & setBitF 7

testBit :: Int -> Getter CPU Word8 -> CPU -> CPU
testBit b reg cpu = 
  cpu & setCpuFlagZ (not . testBitF b $ cpu ^. reg)
    & cpuFlagN .~ False
    & cpuFlagH .~ True

testBitHL :: Int -> CPU -> CPU
testBitHL b = testBit b (mcuLookup cpuRegisterHL)