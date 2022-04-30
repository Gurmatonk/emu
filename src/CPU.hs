{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module CPU (initCpu, runInstruction) where

import CB (cb)
import Clock (updateClock)
import Control.Lens
import Data.Bits (bit, complement, rotateL, rotateR, xor, (.&.), (.|.))
import Data.Bool (bool)
import Data.Int (Int8)
import Data.Tuple (swap)
import Data.Word (Word16, Word8)
import Lenses (cpuFlagC, cpuFlagH, cpuFlagN, cpuFlagZ, cpuRegisterAF, cpuRegisterBC, cpuRegisterDE, cpuRegisterHL, mcuLookup, mcuWrite, pcLookup)
import qualified MCU
import PPU (updatePPU)
import Types
import Utils (bitwiseValue, mkWord16, splitWord16)
import Numeric.Lens (subtracting, adding)

initCpu :: CPU
initCpu =
  CPU
    { _cpuRegisterA = 0x01,
      _cpuRegisterF = 0xB0,
      _cpuRegisterB = 0x00,
      _cpuRegisterC = 0x13,
      _cpuRegisterD = 0x00,
      _cpuRegisterE = 0xD8,
      _cpuRegisterH = 0x01,
      _cpuRegisterL = 0x4D,
      _cpuSP = 0xFFFE,
      _cpuPC = 0x0100,
      _cpuMCU = MCU.initMcu,
      _cpuIME = False
    }

runInstruction :: CPU -> (CPU, Cycles)
runInstruction cpu =
  ( newCpu & cpuMCU . mcuClock %~ updateClock cycles
      & cpuMCU . mcuPPU %~ updatePPU cycles, -- TODO: Interrupt Handling should go here
    cycles
  )
  where
    (newCpu, cycles) = uncurry execInstruction . swap . pcLookup $ cpu

execInstruction :: Word8 -> CPU -> (CPU, Cycles)
execInstruction opcode =
  case opcode of
    0x00 -> (,4) . nop
    0x01 -> (,12) . ldd16 cpuRegisterBC
    0x02 -> (,8) . mcuWrite cpuRegisterBC cpuRegisterA
    0x03 -> (,8) . inc16 cpuRegisterBC
    0x04 -> (,4) . inc cpuRegisterB
    0x05 -> (,4) . dec cpuRegisterB
    0x06 -> (,8) . ldd8 cpuRegisterB
    0x07 -> (,4) . rlcA
    0x08 -> (,20) . lda16SP
    0x09 -> (,8) . addHL cpuRegisterBC
    0x0A -> (,8) . ld cpuRegisterA (mcuLookup cpuRegisterBC)
    0x0B -> (,8) . dec16 cpuRegisterBC
    0x0C -> (,4) . inc cpuRegisterC
    0x0D -> (,4) . dec cpuRegisterC
    0x0E -> (,8) . ldd8 cpuRegisterC
    0x0F -> (,4) . rrcA
    0x10 -> (,4) . stop
    0x11 -> (,12) . ldd16 cpuRegisterDE
    0x12 -> (,8) . mcuWrite cpuRegisterDE cpuRegisterA
    0x13 -> (,8) . inc16 cpuRegisterDE
    0x14 -> (,4) . inc cpuRegisterD
    0x15 -> (,4) . dec cpuRegisterD
    0x16 -> (,8) . ldd8 cpuRegisterD
    0x17 -> (,4) . rlA
    0x18 -> (,8) . jrr8
    0x19 -> (,8) . addHL cpuRegisterDE
    0x1A -> (,8) . ld cpuRegisterA (mcuLookup cpuRegisterDE)
    0x1B -> (,8) . dec16 cpuRegisterDE
    0x1C -> (,4) . inc cpuRegisterE
    0x1D -> (,4) . dec cpuRegisterE
    0x1E -> (,8) . ldd8 cpuRegisterE
    0x1F -> (,4) . rrA
    0x20 -> (,8) . jrNZr8
    0x21 -> (,12) . ldd16 cpuRegisterHL
    0x22 -> (,8) . ldHLplusA
    0x23 -> (,8) . inc16 cpuRegisterHL
    0x24 -> (,4) . inc cpuRegisterH
    0x25 -> (,4) . dec cpuRegisterH
    0x26 -> (,8) . ldd8 cpuRegisterH
    0x27 -> (,4) . daa
    0x28 -> (,8) . jrZr8
    0x29 -> (,8) . addHL cpuRegisterHL
    0x2A -> (,8) . ldAHLplus
    0x2B -> (,8) . dec16 cpuRegisterHL
    0x2C -> (,4) . inc cpuRegisterL
    0x2D -> (,4) . dec cpuRegisterL
    0x2E -> (,8) . ldd8 cpuRegisterL
    0x2F -> (,4) . cpl
    0x30 -> (,8) . jrNCr8
    0x31 -> (,12) . ldd16 cpuSP
    0x32 -> (,8) . ldHLminusA
    0x33 -> (,8) . inc16 cpuSP
    0x34 -> (,12) . incHL_
    0x35 -> (,12) . decHL_
    0x36 -> (,12) . ldHLd8
    0x37 -> (,4) . scf
    0x38 -> (,8) . jrCr8
    0x39 -> (,8) . addHL cpuSP
    0x3A -> (,8) . ldAHLminus
    0x3B -> (,8) . dec16 cpuSP
    0x3C -> (,4) . inc cpuRegisterA
    0x3D -> (,4) . dec cpuRegisterA
    0x3E -> (,8) . ldd8 cpuRegisterA
    0x3F -> (,4) . ccf
    0x40 -> (,4) . ld cpuRegisterB cpuRegisterB
    0x41 -> (,4) . ld cpuRegisterB cpuRegisterC
    0x42 -> (,4) . ld cpuRegisterB cpuRegisterD
    0x43 -> (,4) . ld cpuRegisterB cpuRegisterE
    0x44 -> (,4) . ld cpuRegisterB cpuRegisterH
    0x45 -> (,4) . ld cpuRegisterB cpuRegisterL
    0x46 -> (,8) . ld cpuRegisterB (mcuLookup cpuRegisterHL)
    0x47 -> (,4) . ld cpuRegisterB cpuRegisterA
    0x48 -> (,4) . ld cpuRegisterC cpuRegisterB
    0x49 -> (,4) . ld cpuRegisterC cpuRegisterC
    0x4A -> (,4) . ld cpuRegisterC cpuRegisterD
    0x4B -> (,4) . ld cpuRegisterC cpuRegisterE
    0x4C -> (,4) . ld cpuRegisterC cpuRegisterH
    0x4D -> (,4) . ld cpuRegisterC cpuRegisterL
    0x4E -> (,8) . ld cpuRegisterC (mcuLookup cpuRegisterHL)
    0x4F -> (,4) . ld cpuRegisterC cpuRegisterA
    0x50 -> (,4) . ld cpuRegisterD cpuRegisterB
    0x51 -> (,4) . ld cpuRegisterD cpuRegisterC
    0x52 -> (,4) . ld cpuRegisterD cpuRegisterD
    0x53 -> (,4) . ld cpuRegisterD cpuRegisterE
    0x54 -> (,4) . ld cpuRegisterD cpuRegisterH
    0x55 -> (,4) . ld cpuRegisterD cpuRegisterL
    0x56 -> (,8) . ld cpuRegisterD (mcuLookup cpuRegisterHL)
    0x57 -> (,4) . ld cpuRegisterD cpuRegisterA
    0x58 -> (,4) . ld cpuRegisterE cpuRegisterB
    0x59 -> (,4) . ld cpuRegisterE cpuRegisterC
    0x5A -> (,4) . ld cpuRegisterE cpuRegisterD
    0x5B -> (,4) . ld cpuRegisterE cpuRegisterE
    0x5C -> (,4) . ld cpuRegisterE cpuRegisterH
    0x5D -> (,4) . ld cpuRegisterE cpuRegisterL
    0x5E -> (,8) . ld cpuRegisterE (mcuLookup cpuRegisterHL)
    0x5F -> (,4) . ld cpuRegisterE cpuRegisterA
    0x60 -> (,4) . ld cpuRegisterH cpuRegisterB
    0x61 -> (,4) . ld cpuRegisterH cpuRegisterC
    0x62 -> (,4) . ld cpuRegisterH cpuRegisterD
    0x63 -> (,4) . ld cpuRegisterH cpuRegisterE
    0x64 -> (,4) . ld cpuRegisterH cpuRegisterH
    0x65 -> (,4) . ld cpuRegisterH cpuRegisterL
    0x66 -> (,8) . ld cpuRegisterH (mcuLookup cpuRegisterHL)
    0x67 -> (,4) . ld cpuRegisterH cpuRegisterA
    0x68 -> (,4) . ld cpuRegisterL cpuRegisterB
    0x69 -> (,4) . ld cpuRegisterL cpuRegisterC
    0x6A -> (,4) . ld cpuRegisterL cpuRegisterD
    0x6B -> (,4) . ld cpuRegisterL cpuRegisterE
    0x6C -> (,4) . ld cpuRegisterL cpuRegisterH
    0x6D -> (,4) . ld cpuRegisterL cpuRegisterL
    0x6E -> (,8) . ld cpuRegisterL (mcuLookup cpuRegisterHL)
    0x6F -> (,4) . ld cpuRegisterL cpuRegisterA
    0x70 -> (,8) . mcuWrite cpuRegisterHL cpuRegisterB
    0x71 -> (,8) . mcuWrite cpuRegisterHL cpuRegisterC
    0x72 -> (,8) . mcuWrite cpuRegisterHL cpuRegisterD
    0x73 -> (,8) . mcuWrite cpuRegisterHL cpuRegisterE
    0x74 -> (,8) . mcuWrite cpuRegisterHL cpuRegisterH
    0x75 -> (,8) . mcuWrite cpuRegisterHL cpuRegisterL
    0x76 -> (,4) . halt
    0x77 -> (,8) . mcuWrite cpuRegisterHL cpuRegisterA
    0x78 -> (,4) . ld cpuRegisterA cpuRegisterB
    0x79 -> (,4) . ld cpuRegisterA cpuRegisterC
    0x7A -> (,4) . ld cpuRegisterA cpuRegisterD
    0x7B -> (,4) . ld cpuRegisterA cpuRegisterE
    0x7C -> (,4) . ld cpuRegisterA cpuRegisterH
    0x7D -> (,4) . ld cpuRegisterA cpuRegisterL
    0x7E -> (,8) . ld cpuRegisterA (mcuLookup cpuRegisterHL)
    0x7F -> (,4) . ld cpuRegisterA cpuRegisterA
    0x80 -> (,4) . addA cpuRegisterB
    0x81 -> (,4) . addA cpuRegisterC
    0x82 -> (,4) . addA cpuRegisterD
    0x83 -> (,4) . addA cpuRegisterE
    0x84 -> (,4) . addA cpuRegisterH
    0x85 -> (,4) . addA cpuRegisterL
    0x86 -> (,8) . addA (mcuLookup cpuRegisterHL)
    0x87 -> (,4) . addA cpuRegisterA
    0x88 -> (,4) . adcA cpuRegisterB
    0x89 -> (,4) . adcA cpuRegisterC
    0x8A -> (,4) . adcA cpuRegisterD
    0x8B -> (,4) . adcA cpuRegisterE
    0x8C -> (,4) . adcA cpuRegisterH
    0x8D -> (,4) . adcA cpuRegisterL
    0x8E -> (,8) . adcA (mcuLookup cpuRegisterHL)
    0x8F -> (,4) . adcA cpuRegisterA
    0x90 -> (,4) . sub cpuRegisterB
    0x91 -> (,4) . sub cpuRegisterC
    0x92 -> (,4) . sub cpuRegisterD
    0x93 -> (,4) . sub cpuRegisterE
    0x94 -> (,4) . sub cpuRegisterH
    0x95 -> (,4) . sub cpuRegisterL
    0x96 -> (,8) . sub (mcuLookup cpuRegisterHL)
    0x97 -> (,4) . sub cpuRegisterA
    0x98 -> (,4) . sbcB
    0x99 -> (,4) . sbcC
    0x9A -> (,4) . sbcD
    0x9B -> (,4) . sbcE
    0x9C -> (,4) . sbcH
    0x9D -> (,4) . sbcL
    0x9E -> (,8) . sbcHL
    0x9F -> (,4) . sbcA
    0xA0 -> (,4) . andB
    0xA1 -> (,4) . andC
    0xA2 -> (,4) . andD
    0xA3 -> (,4) . andE
    0xA4 -> (,4) . andH
    0xA5 -> (,4) . andL
    0xA6 -> (,8) . andHL
    0xA7 -> (,4) . andA
    0xA8 -> (,4) . xorB
    0xA9 -> (,4) . xorC
    0xAA -> (,4) . xorD
    0xAB -> (,4) . xorE
    0xAC -> (,4) . xorH
    0xAD -> (,4) . xorL
    0xAE -> (,8) . xorHL
    0xAF -> (,4) . xorA
    0xB0 -> (,4) . orB
    0xB1 -> (,4) . orC
    0xB2 -> (,4) . orD
    0xB3 -> (,4) . orE
    0xB4 -> (,4) . orH
    0xB5 -> (,4) . orL
    0xB6 -> (,8) . orHL
    0xB7 -> (,4) . orA
    0xB8 -> (,4) . cpB
    0xB9 -> (,4) . cpC
    0xBA -> (,4) . cpD
    0xBB -> (,4) . cpE
    0xBC -> (,4) . cpH
    0xBD -> (,4) . cpL
    0xBE -> (,8) . cpHL
    0xBF -> (,4) . cpA
    0xC0 -> (,8) . retNZ
    0xC1 -> (,12) . popBC
    0xC2 -> (,12) . jpNZa16
    0xC3 -> (,12) . jpa16
    0xC4 -> (,12) . callNZa16
    0xC5 -> (,16) . pushBC
    0xC6 -> (,8) . addAd8
    0xC7 -> (,32) . rst00
    0xC8 -> (,8) . retZ
    0xC9 -> (,8) . ret
    0xCA -> (,12) . jpZa16
    0xCB -> cb
    0xCC -> (,12) . callZa16
    0xCD -> (,12) . calla16
    0xCE -> (,8) . adcAd8
    0xCF -> (,32) . rst08
    0xD0 -> (,8) . retNC
    0xD1 -> (,12) . popDE
    0xD2 -> (,12) . jpNCa16
    0xD4 -> (,12) . callNCa16
    0xD5 -> (,16) . pushDE
    0xD6 -> (,8) . subd8
    0xD7 -> (,32) . rst10
    0xD8 -> (,8) . retC
    0xD9 -> (,8) . reti
    0xDA -> (,12) . jpCa16
    0xDC -> (,12) . callCa16
    0xDE -> (,8) . sbcAd8 -- CPU Manual gave ?? for this ...
    0xDF -> (,32) . rst18
    0xE0 -> (,12) . ldha8A
    0xE1 -> (,12) . popHL
    0xE2 -> (,8) . ldhCA
    0xE5 -> (,16) . pushHL
    0xE6 -> (,8) . andd8
    0xE7 -> (,32) . rst20
    0xE8 -> (,16) . addSPr8
    0xE9 -> (,4) . jpHL
    0xEA -> (,16) . lda16A
    0xEE -> (,8) . xord8
    0xEF -> (,32) . rst28
    0xF0 -> (,12) . ldhAa8
    0xF1 -> (,12) . popAF
    0xF2 -> (,8) . ldhAC
    0xF3 -> (,4) . di
    0xF5 -> (,16) . pushAF
    0xF6 -> (,8) . ord8
    0xF7 -> (,32) . rst30
    0xF8 -> (,12) . ldHLSPplusr8
    0xF9 -> (,8) . ldSPHL
    0xFA -> (,16) . ldAa16
    0xFB -> (,4) . ei
    0xFE -> (,8) . cpd8
    0xFF -> (,32) . rst38
    _ -> undefined

nop :: CPU -> CPU
nop = id

-- TODO: Halt until interrupt occurs!
halt :: CPU -> CPU
halt cpu = undefined

-- TODO: Low power standby - whatever that is
-- TODO: Reset AND STOP clockDivider (FF04)
-- NOTE: From the specs this should encode as 0x1000, i.e. 2 bytes - since this is unnecessary it is 'sometimes' encoded simply as 0x10
--       - might need to check whether PC should be incremented by 1 or 2....
stop :: CPU -> CPU
stop cpu =
  undefined & cpuPC +~ 1 -- TODO: Extra inc according to spec. Delete, maybe...

-- Note: Mainly transliterated from https://forums.nesdev.org/viewtopic.php?t=15944
--       I have no idea what I'm doing
daa :: CPU -> CPU
daa cpu =
  cpu & correctA
    & (\cpu' -> cpu' & cpuFlagZ .~ (cpu' ^. cpuRegisterA == 0))
    & cpuFlagH .~ False
  where
    correctA cpu' =
      if cpu' ^. cpuFlagN
        then cpu' & cpuRegisterA -~ bool 0x00 0x60 (cpu' ^. cpuFlagC) & cpuRegisterA -~ bool 0x00 0x6 (cpu' ^. cpuFlagH)
        else
          cpu' &
            (\cpu'' -> if cpu'' ^. cpuFlagC || cpu'' ^. cpuRegisterA > 0x99
              then cpu'' & cpuRegisterA +~ 0x60 & cpuFlagC .~ True
              else cpu''
            )
            & (\cpu'' -> if cpu'' ^. cpuFlagH || (cpu'' ^. cpuRegisterA .&. 0x0F) > 0x09
                then cpu'' & cpuRegisterA +~ 0x6
                else cpu''
            )


ld :: Setter' CPU Word8 -> Getter CPU Word8 -> CPU -> CPU
ld str gtr cpu = cpu & str .~ (cpu ^. gtr)

ldHLplusA :: CPU -> CPU
ldHLplusA cpu =
  cpu & mcuWrite cpuRegisterHL cpuRegisterA
    & cpuRegisterHL +~ 1

ldHLminusA :: CPU -> CPU
ldHLminusA cpu =
  cpu & mcuWrite cpuRegisterHL cpuRegisterA
    & cpuRegisterHL -~ 1

ldAHLplus :: CPU -> CPU
ldAHLplus cpu =
  cpu & cpuRegisterA .~ (cpu ^. mcuLookup cpuRegisterHL)
    & cpuRegisterHL +~ 1

ldAHLminus :: CPU -> CPU
ldAHLminus cpu =
  cpu & cpuRegisterA .~ (cpu ^. mcuLookup cpuRegisterHL)
    & cpuRegisterHL -~ 1

ldd8 :: Lens' CPU Word8 -> CPU -> CPU
ldd8 reg cpu = cpu' & reg .~ d8
  where
    (cpu', d8) = pcLookup cpu

ldHLd8 :: CPU -> CPU
ldHLd8 cpu = mcuWrite cpuRegisterHL (to . const $ d8) cpu'
  where
    (cpu', d8) = pcLookup cpu

ldd16 :: Lens' CPU Word16 -> CPU -> CPU
ldd16 reg cpu = cpu'' & reg .~ mkWord16 msb lsb
  where
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

lda16SP :: CPU -> CPU
lda16SP cpu = 
  mcuWrite (to . const $ mkWord16 msb lsb + 1) (to . const $ spmsb) 
    . mcuWrite (to . const $ mkWord16 msb lsb) (to . const $ splsb) $ cpu''
  where
    (spmsb, splsb) = splitWord16 (cpu ^. cpuSP)
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

lda16A :: CPU -> CPU
lda16A cpu = mcuWrite (to . const $ mkWord16 msb lsb) cpuRegisterA cpu''
  where
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

ldAa16 :: CPU -> CPU
ldAa16 cpu = cpu'' & cpuRegisterA .~ (cpu'' ^. mcuLookup (to . const $ mkWord16 msb lsb))
  where
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

ldSPHL :: CPU -> CPU
ldSPHL cpu = cpu & cpuSP .~ (cpu ^. cpuRegisterHL)

inc16 :: Setter' CPU Word16 -> CPU -> CPU
inc16 reg = reg +~ 1

dec16 :: Setter' CPU Word16 -> CPU -> CPU
dec16 reg = reg -~ 1

-- NOTE: The rotation operations from the 1-Byte instruction set differ from those in CB in that - according to the docs -
--       CB rotations CAN set the Z flag, while these here by definition just reset it.
rlA :: CPU -> CPU
rlA cpu =
  cpu & cpuRegisterA . bitwiseValue (bit 7) .~ new7th -- swap 7th bit with carry so we can ignore carry when rotating
    & cpuFlagC .~ old7th
    & cpuRegisterA %~ (`rotateL` 1)
    & cpuFlagZ .~ False
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old7th = cpu ^. cpuRegisterA . bitwiseValue (bit 7)
    new7th = cpu ^. cpuFlagC

rrA :: CPU -> CPU
rrA cpu =
  cpu & cpuRegisterA . bitwiseValue (bit 0) .~ new0th -- swap 0th bit with carry so we can ignore carry when rotating
    & cpuFlagC .~ old0th
    & cpuRegisterA %~ (`rotateR` 1)
    & cpuFlagZ .~ False
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old0th = cpu ^. cpuRegisterA . bitwiseValue (bit 0)
    new0th = cpu ^. cpuFlagC

rlcA :: CPU -> CPU
rlcA cpu =
  cpu & cpuFlagC .~ old7th
    & cpuRegisterA %~ (`rotateL` 1)
    & cpuFlagZ .~ False
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old7th = cpu ^. cpuRegisterA . bitwiseValue (bit 7)

rrcA :: CPU -> CPU
rrcA cpu =
  cpu & cpuFlagC .~ old0th
    & cpuRegisterA %~ (`rotateR` 1)
    & cpuFlagZ .~ False
    & cpuFlagN .~ False
    & cpuFlagH .~ False
  where
    old0th = cpu ^. cpuRegisterA . bitwiseValue (bit 0)

inc :: Lens' CPU Word8 -> CPU -> CPU
inc reg cpu =
  cpu & reg +~ 1
    & cpuFlagH .~ (0x0F .&. cpu ^. reg + 1 > 0x0F)
    & cpuFlagZ .~ (cpu ^. reg + 1 == 0x00)
    & cpuFlagN .~ False

incHL_ :: CPU -> CPU
incHL_ cpu =
  cpu & mcuWrite cpuRegisterHL val
    & cpuFlagH .~ (0x0F .&. (cpu ^. val) > 0x0F)
    & cpuFlagZ .~ (cpu ^. val == 0x00)
    & cpuFlagN .~ False
  where
    val :: Getter CPU Word8
    val = mcuLookup cpuRegisterHL . adding 1

dec :: Lens' CPU Word8 -> CPU -> CPU
dec reg cpu =
  cpu & reg -~ 1
    & cpuFlagH .~ (0x0F .&. cpu ^. reg == 0x00)
    & cpuFlagZ .~ (cpu ^. reg - 1 == 0x00)
    & cpuFlagN .~ True

decHL_ :: CPU -> CPU
decHL_ cpu =
  cpu & mcuWrite cpuRegisterHL (val . subtracting 1)
    & cpuFlagH .~ (0x0F .&. (cpu ^. val) == 0x00)
    & cpuFlagZ .~ ((cpu ^. val . subtracting 1) == 0x00)
    & cpuFlagN .~ True
  where
    val :: Getter CPU Word8
    val = mcuLookup cpuRegisterHL

addHL :: Lens' CPU Word16 -> CPU -> CPU
addHL reg cpu =
  cpu & cpuRegisterHL .~ op1 + op2
    & cpuFlagC .~ (op1 + op2 < op1)
    & cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
    & cpuFlagN .~ False
  where
    op1 = cpu ^. cpuRegisterHL
    op2 = cpu ^. reg

addSPr8 :: CPU -> CPU
addSPr8 cpu =
  ( if r8 >= 0
      then
        cpu' & cpuSP .~ op1 + op2
          & cpuFlagC .~ (op1 + op2 < op1)
          & cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
      else
        cpu' & cpuSP .~ op1 - op2
          & cpuFlagC .~ (op1 - op2 > op1)
          & cpuFlagH .~ (0x0FFF .&. op1 - 0x0FFF .&. op2 < 0x0FFF) -- TODO: Is this correct??
  )
    & cpuFlagZ .~ False
    & cpuFlagN .~ False
  where
    (cpu', r8) = fromIntegral <$> pcLookup cpu :: (CPU, Int8)
    op1 = cpu ^. cpuSP
    op2 = fromIntegral (abs r8)

ldHLSPplusr8 :: CPU -> CPU
ldHLSPplusr8 cpu =
  ( if r8 >= 0
      then
        cpu' & cpuRegisterHL .~ op1 + op2
          & cpuFlagC .~ (op1 + op2 < op1)
          & cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
      else
        cpu' & cpuRegisterHL .~ op1 - op2
          & cpuFlagC .~ (op1 - op2 > op1)
          & cpuFlagH .~ (0x0FFF .&. op1 - 0x0FFF .&. op2 < 0x0FFF) -- TODO: Is this correct??
  )
    & cpuFlagZ .~ False
    & cpuFlagN .~ False
  where
    (cpu', r8) = fromIntegral <$> pcLookup cpu :: (CPU, Int8)
    op1 = cpu ^. cpuSP
    op2 = fromIntegral (abs r8)

addA :: Getting Word8 CPU Word8 -> CPU -> CPU
addA reg cpu =
  cpu & cpuRegisterA .~ op1 + op2
    & cpuFlagZ .~ (op1 + op2 == 0x00)
    & cpuFlagH .~ (op1 + op2 < op1) -- I suppose H in this context is just C...?
    & cpuFlagC .~ (op1 + op2 < op1)
    & cpuFlagN .~ False
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg

addAd8 :: CPU -> CPU
addAd8 cpu =
  cpu' & cpuRegisterA .~ op1 + op2
    & cpuFlagZ .~ (op1 + op2 == 0x00)
    & cpuFlagH .~ (op1 + op2 < op1)
    & cpuFlagC .~ (op1 + op2 < op1)
    & cpuFlagN .~ False
  where
    op1 = cpu ^. cpuRegisterA
    (cpu', op2) = pcLookup cpu

adcA :: Getting Word8 CPU Word8 -> CPU -> CPU
adcA reg cpu =
  cpu & cpuRegisterA .~ op1 + op2 + cy
    & cpuFlagZ .~ (op1 + op2 + cy == 0x00)
    & cpuFlagH .~ (op1 + op2 < op1 || (op1 + op2 + cy < op1 + op2)) -- I suppose H in this context is just C...?
    & cpuFlagC .~ (op1 + op2 < op1 || (op1 + op2 + cy < op1 + op2)) -- FIXME: There must be sth more elegant :/
    & cpuFlagN .~ False
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg
    cy = bool 0 1 $ cpu ^. cpuFlagC

adcAd8 :: CPU -> CPU
adcAd8 cpu = cpu' & adcA (to (const d8))
  where
    (cpu', d8) = pcLookup cpu

sub :: Getting Word8 CPU Word8 -> CPU -> CPU
sub reg cpu =
  cpu & cpuRegisterA .~ op1 - op2
    & cpuFlagZ .~ (op1 - op2 == 0x00)
    & cpuFlagH .~ (op1 - op2 > op1) -- I suppose H in this context is just C...?
    & cpuFlagC .~ (op1 - op2 > op1)
    & cpuFlagN .~ True
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg

subd8 :: CPU -> CPU
subd8 cpu = cpu' & sub (to (const d8))
  where
    (cpu', d8) = pcLookup cpu

sbc :: Getting Word8 CPU Word8 -> CPU -> CPU
sbc reg cpu =
  cpu & cpuRegisterA .~ op1 - op2 - cy
    & cpuFlagZ .~ (op1 - op2 - cy == 0x00)
    & cpuFlagH .~ (op1 - op2 > op1 || op1 - op2 - cy > op1 - op2) -- I suppose H in this context is just C...?
    & cpuFlagC .~ (op1 - op2 > op1 || op1 - op2 - cy > op1 - op2)
    & cpuFlagN .~ True
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg
    cy = bool 0 1 $ cpu ^. cpuFlagC

sbcB :: CPU -> CPU
sbcB = sbc cpuRegisterB

sbcC :: CPU -> CPU
sbcC = sbc cpuRegisterC

sbcD :: CPU -> CPU
sbcD = sbc cpuRegisterD

sbcE :: CPU -> CPU
sbcE = sbc cpuRegisterE

sbcH :: CPU -> CPU
sbcH = sbc cpuRegisterH

sbcL :: CPU -> CPU
sbcL = sbc cpuRegisterL

sbcHL :: CPU -> CPU
sbcHL = sbc (mcuLookup cpuRegisterHL)

sbcA :: CPU -> CPU
sbcA = sbc cpuRegisterA

sbcAd8 :: CPU -> CPU
sbcAd8 cpu = cpu' & sbc (to (const d8))
  where
    (cpu', d8) = pcLookup cpu

aAnd :: Getting Word8 CPU Word8 -> CPU -> CPU
aAnd reg cpu =
  cpu & cpuRegisterA .~ op1 .&. op2
    & cpuFlagZ .~ (op1 .&. op2 == 0x00)
    & cpuFlagN .~ False
    & cpuFlagH .~ True
    & cpuFlagC .~ False
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg

andB :: CPU -> CPU
andB = aAnd cpuRegisterB

andC :: CPU -> CPU
andC = aAnd cpuRegisterC

andD :: CPU -> CPU
andD = aAnd cpuRegisterD

andE :: CPU -> CPU
andE = aAnd cpuRegisterE

andH :: CPU -> CPU
andH = aAnd cpuRegisterH

andL :: CPU -> CPU
andL = aAnd cpuRegisterL

andHL :: CPU -> CPU
andHL = aAnd (mcuLookup cpuRegisterHL)

andA :: CPU -> CPU
andA = aAnd cpuRegisterA

andd8 :: CPU -> CPU
andd8 cpu = cpu' & aAnd (to (const d8))
  where
    (cpu', d8) = pcLookup cpu

aXor :: Getting Word8 CPU Word8 -> CPU -> CPU
aXor reg cpu =
  cpu & cpuRegisterA .~ op1 `xor` op2
    & cpuFlagZ .~ (op1 `xor` op2 == 0x00)
    & cpuFlagN .~ False
    & cpuFlagH .~ False
    & cpuFlagC .~ False
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg

xorB :: CPU -> CPU
xorB = aXor cpuRegisterB

xorC :: CPU -> CPU
xorC = aXor cpuRegisterC

xorD :: CPU -> CPU
xorD = aXor cpuRegisterD

xorE :: CPU -> CPU
xorE = aXor cpuRegisterE

xorH :: CPU -> CPU
xorH = aXor cpuRegisterH

xorL :: CPU -> CPU
xorL = aXor cpuRegisterL

xorHL :: CPU -> CPU
xorHL = aXor (mcuLookup cpuRegisterHL)

xorA :: CPU -> CPU
xorA = aXor cpuRegisterA

xord8 :: CPU -> CPU
xord8 cpu = cpu' & aXor (to (const d8))
  where
    (cpu', d8) = pcLookup cpu

aor :: Getting Word8 CPU Word8 -> CPU -> CPU
aor reg cpu =
  cpu & cpuRegisterA .~ op1 .|. op2
    & cpuFlagZ .~ (op1 .|. op2 == 0x00)
    & cpuFlagN .~ False
    & cpuFlagH .~ False
    & cpuFlagC .~ False
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg

orB :: CPU -> CPU
orB = aor cpuRegisterB

orC :: CPU -> CPU
orC = aor cpuRegisterC

orD :: CPU -> CPU
orD = aor cpuRegisterD

orE :: CPU -> CPU
orE = aor cpuRegisterE

orH :: CPU -> CPU
orH = aor cpuRegisterH

orL :: CPU -> CPU
orL = aor cpuRegisterL

orHL :: CPU -> CPU
orHL = aor (mcuLookup cpuRegisterHL)

orA :: CPU -> CPU
orA = aor cpuRegisterA

ord8 :: CPU -> CPU
ord8 cpu = cpu' & aor (to (const d8))
  where
    (cpu', d8) = pcLookup cpu

-- NOTE: This is just sub without writing into A. Consider separation of flag effects, operations, and actual writing of registers?
acp :: Getting Word8 CPU Word8 -> CPU -> CPU
acp reg cpu =
  cpu & cpuFlagZ .~ (op1 == op2)
    & cpuFlagN .~ True
    & cpuFlagH .~ (op1 - op2 > op1)
    & cpuFlagC .~ (op1 - op2 > op1)
  where
    op1 = cpu ^. cpuRegisterA
    op2 = cpu ^. reg

cpB :: CPU -> CPU
cpB = acp cpuRegisterB

cpC :: CPU -> CPU
cpC = acp cpuRegisterC

cpD :: CPU -> CPU
cpD = acp cpuRegisterD

cpE :: CPU -> CPU
cpE = acp cpuRegisterE

cpH :: CPU -> CPU
cpH = acp cpuRegisterH

cpL :: CPU -> CPU
cpL = acp cpuRegisterL

cpHL :: CPU -> CPU
cpHL = acp (mcuLookup cpuRegisterHL)

cpA :: CPU -> CPU
cpA = acp cpuRegisterA

cpd8 :: CPU -> CPU
cpd8 cpu = cpu' & acp (to (const d8))
  where
    (cpu', d8) = pcLookup cpu

jrr8 :: CPU -> CPU
jrr8 cpu =
  cpu'
    & ( if r8 < 0
          then cpuPC -~ fromIntegral (abs r8)
          else cpuPC +~ fromIntegral r8
      )
  where
    (cpu', r8) = fromIntegral <$> pcLookup cpu :: (CPU, Int8)

-- TODO: Consider whether implementation in terms of jrr8 is a good idea or not...
jrZr8 :: CPU -> CPU
jrZr8 cpu =
  if cpu ^. cpuFlagZ
    then jrr8 cpu
    else fst . pcLookup $ cpu -- read it and throw it away...

jrNZr8 :: CPU -> CPU
jrNZr8 cpu =
  if not (cpu ^. cpuFlagZ)
    then jrr8 cpu
    else fst . pcLookup $ cpu

jrCr8 :: CPU -> CPU
jrCr8 cpu =
  if cpu ^. cpuFlagC
    then jrr8 cpu
    else fst . pcLookup $ cpu

jrNCr8 :: CPU -> CPU
jrNCr8 cpu =
  if not (cpu ^. cpuFlagC)
    then jrr8 cpu
    else fst . pcLookup $ cpu

jpHL :: CPU -> CPU
jpHL cpu =
  cpu & cpuPC .~ (cpu ^. cpuRegisterHL)

jpa16 :: CPU -> CPU
jpa16 cpu = cpu'' & cpuPC .~ mkWord16 msb lsb
  where
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

jpZa16 :: CPU -> CPU
jpZa16 cpu =
  if cpu ^. cpuFlagZ
    then jpa16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

jpNZa16 :: CPU -> CPU
jpNZa16 cpu =
  if not (cpu ^. cpuFlagZ)
    then jpa16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

jpCa16 :: CPU -> CPU
jpCa16 cpu =
  if cpu ^. cpuFlagC
    then jpa16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

jpNCa16 :: CPU -> CPU
jpNCa16 cpu =
  if not (cpu ^. cpuFlagC)
    then jpa16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

cpl :: CPU -> CPU
cpl cpu =
  cpu & cpuRegisterA %~ complement
    & cpuFlagN .~ True
    & cpuFlagH .~ True

scf :: CPU -> CPU
scf cpu =
  cpu & cpuFlagC .~ True
    & cpuFlagN .~ False
    & cpuFlagH .~ False

ccf :: CPU -> CPU
ccf cpu =
  cpu & cpuFlagC %~ not
    & cpuFlagN .~ False
    & cpuFlagH .~ False

ret :: CPU -> CPU
ret cpu =
  cpu & cpuPC .~ mkWord16 msb lsb
    & cpuSP +~ 2
  where
    lsb = cpu ^. mcuLookup cpuSP
    msb = cpu ^. mcuLookup (cpuSP . adding 1)

retZ :: CPU -> CPU
retZ cpu =
  if cpu ^. cpuFlagZ
    then ret cpu
    else cpu

retNZ :: CPU -> CPU
retNZ cpu =
  if not (cpu ^. cpuFlagZ)
    then ret cpu
    else cpu

retC :: CPU -> CPU
retC cpu =
  if cpu ^. cpuFlagC
    then ret cpu
    else cpu

retNC :: CPU -> CPU
retNC cpu =
  if not (cpu ^. cpuFlagC)
    then ret cpu
    else cpu

pop :: Lens' CPU Word16 -> CPU -> CPU
pop reg cpu =
  cpu & reg .~ mkWord16 msb lsb
    & cpuSP +~ 2
  where
    lsb = cpu ^. mcuLookup cpuSP
    msb = cpu ^. mcuLookup (cpuSP . adding 1)

popBC :: CPU -> CPU
popBC = pop cpuRegisterBC

popDE :: CPU -> CPU
popDE = pop cpuRegisterDE

popHL :: CPU -> CPU
popHL = pop cpuRegisterHL

popAF :: CPU -> CPU
popAF = pop cpuRegisterAF

push :: Lens' CPU Word16 -> CPU -> CPU
push reg cpu =
  cpu & mcuWrite (cpuSP . subtracting 1) (reg . to splitWord16 . _1)
    & mcuWrite (cpuSP . subtracting 2) (reg . to splitWord16 . _2)
    & cpuSP -~ 2

pushBC :: CPU -> CPU
pushBC = push cpuRegisterBC

pushDE :: CPU -> CPU
pushDE = push cpuRegisterDE

pushHL :: CPU -> CPU
pushHL = push cpuRegisterHL

pushAF :: CPU -> CPU
pushAF = push cpuRegisterAF

calla16 :: CPU -> CPU
calla16 cpu =
  cpu'' & mcuWrite (cpuSP . subtracting 1) (cpuPC . to splitWord16 . _1)
    & mcuWrite (cpuSP . subtracting 2) (cpuPC . to splitWord16 . _2)
    & cpuSP -~ 2
    & cpuPC .~ mkWord16 targetmsb targetlsb
  where
    (cpu', targetlsb) = pcLookup cpu
    (cpu'', targetmsb) = pcLookup cpu'

callZa16 :: CPU -> CPU
callZa16 cpu =
  if cpu ^. cpuFlagZ
    then calla16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

callNZa16 :: CPU -> CPU
callNZa16 cpu =
  if not (cpu ^. cpuFlagZ)
    then calla16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

callCa16 :: CPU -> CPU
callCa16 cpu =
  if cpu ^. cpuFlagC
    then calla16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

callNCa16 :: CPU -> CPU
callNCa16 cpu =
  if not (cpu ^. cpuFlagC)
    then calla16 cpu
    else fst . pcLookup . fst . pcLookup $ cpu

rst :: Word8 -> CPU -> CPU
rst lsb cpu =
  cpu & mcuWrite (cpuSP . subtracting 1) (cpuPC . to splitWord16 . _1)
    & mcuWrite (cpuSP . subtracting 2) (cpuPC . to splitWord16 . _2)
    & cpuSP -~ 2
    & cpuPC .~ mkWord16 0x00 lsb

rst00 :: CPU -> CPU
rst00 = rst 0x00

rst08 :: CPU -> CPU
rst08 = rst 0x08

rst10 :: CPU -> CPU
rst10 = rst 0x10

rst18 :: CPU -> CPU
rst18 = rst 0x18

rst20 :: CPU -> CPU
rst20 = rst 0x20

rst28 :: CPU -> CPU
rst28 = rst 0x28

rst30 :: CPU -> CPU
rst30 = rst 0x30

rst38 :: CPU -> CPU
rst38 = rst 0x38

ei :: CPU -> CPU
ei cpu = cpu & cpuIME .~ True

di :: CPU -> CPU
di cpu = cpu & cpuIME .~ False

reti :: CPU -> CPU
reti = ret . ei

-- Note: The pandocs classify ldhCA and ldhAC as 2-Byte-instructions - however it is not at all clear where the read of the 2nd byte should occur
--       This being two byte is either a bug in the cpu or a mistake in the pandocs - currently going for the latter
ldhCA :: CPU -> CPU
ldhCA cpu = mcuWrite (to . const $ mkWord16 0xFF lsb) cpuRegisterA cpu
  where
    lsb = cpu ^. cpuRegisterC

ldhAC :: CPU -> CPU
ldhAC cpu = cpu & cpuRegisterA .~ newVal
  where
    newVal = cpu ^. mcuLookup (to . const $ mkWord16 0xFF lsb)
    lsb = cpu ^. cpuRegisterC

ldha8A :: CPU -> CPU
ldha8A cpu = cpu' & mcuWrite (to . const $ mkWord16 0xFF lsb) cpuRegisterA
  where
    (cpu', lsb) = pcLookup cpu

ldhAa8 :: CPU -> CPU
ldhAa8 cpu = cpu' & cpuRegisterA .~ newVal
  where
    newVal = cpu' ^. mcuLookup (to . const $ mkWord16 0xFF lsb)
    (cpu', lsb) = pcLookup cpu
