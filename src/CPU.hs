{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module CPU where

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
    0x01 -> (,12) . ldBCd16
    0x02 -> (,8) . ldBCA
    0x03 -> (,8) . incBC
    0x04 -> (,4) . incB
    0x05 -> (,4) . decB
    0x06 -> (,8) . ldBd8
    0x07 -> (,4) . rlcA
    0x08 -> (,20) . lda16SP
    0x09 -> (,8) . addHLBC
    0x0A -> (,8) . ldABC
    0x0B -> (,8) . decBC
    0x0C -> (,4) . incC
    0x0D -> (,4) . decC
    0x0E -> (,8) . ldCd8
    0x0F -> (,4) . rrcA
    0x10 -> (,4) . stop
    0x11 -> (,12) . ldDEd16
    0x12 -> (,8) . ldDEA
    0x13 -> (,8) . incDE
    0x14 -> (,4) . incD
    0x15 -> (,4) . decD
    0x16 -> (,8) . ldDd8
    0x17 -> (,4) . rlA
    0x18 -> (,8) . jrr8
    0x19 -> (,8) . addHLDE
    0x1A -> (,8) . ldADE
    0x1B -> (,8) . decDE
    0x1C -> (,4) . incE
    0x1D -> (,4) . decE
    0x1E -> (,8) . ldEd8
    0x1F -> (,4) . rrA
    0x20 -> (,8) . jrNZr8
    0x21 -> (,12) . ldHLd16
    0x22 -> (,8) . ldHLplusA
    0x23 -> (,8) . incHL
    0x24 -> (,4) . incH
    0x25 -> (,4) . decH
    0x26 -> (,8) . ldHd8
    0x27 -> (,4) . daa
    0x28 -> (,8) . jrZr8
    0x29 -> (,8) . addHLHL
    0x2A -> (,8) . ldAHLplus
    0x2B -> (,8) . decHL
    0x2C -> (,4) . incL
    0x2D -> (,4) . decL
    0x2E -> (,8) . ldLd8
    0x2F -> (,4) . cpl
    0x30 -> (,8) . jrNCr8
    0x31 -> (,12) . ldSPd16
    0x32 -> (,8) . ldHLminusA
    0x33 -> (,8) . incSP
    0x34 -> (,12) . incHL_
    0x35 -> (,12) . decHL_
    0x36 -> (,12) . ldHLd8
    0x37 -> (,4) . scf
    0x38 -> (,8) . jrCr8
    0x39 -> (,8) . addHLSP
    0x3A -> (,8) . ldAHLminus
    0x3B -> (,8) . decSP
    0x3C -> (,4) . incA
    0x3D -> (,4) . decA
    0x3E -> (,8) . ldAd8
    0x3F -> (,4) . ccf
    0x40 -> (,4) . ldBB
    0x41 -> (,4) . ldBC
    0x42 -> (,4) . ldBD
    0x43 -> (,4) . ldBE
    0x44 -> (,4) . ldBH
    0x45 -> (,4) . ldBL
    0x46 -> (,8) . ldBHL
    0x47 -> (,4) . ldBA
    0x48 -> (,4) . ldCB
    0x49 -> (,4) . ldCC
    0x4A -> (,4) . ldCD
    0x4B -> (,4) . ldCE
    0x4C -> (,4) . ldCH
    0x4D -> (,4) . ldCL
    0x4E -> (,8) . ldCHL
    0x4F -> (,4) . ldCA
    0x50 -> (,4) . ldDB
    0x51 -> (,4) . ldDC
    0x52 -> (,4) . ldDD
    0x53 -> (,4) . ldDE
    0x54 -> (,4) . ldDH
    0x55 -> (,4) . ldDL
    0x56 -> (,8) . ldDHL
    0x57 -> (,4) . ldDA
    0x58 -> (,4) . ldEB
    0x59 -> (,4) . ldEC
    0x5A -> (,4) . ldED
    0x5B -> (,4) . ldEE
    0x5C -> (,4) . ldEH
    0x5D -> (,4) . ldEL
    0x5E -> (,8) . ldEHL
    0x5F -> (,4) . ldEA
    0x60 -> (,4) . ldHB
    0x61 -> (,4) . ldHC
    0x62 -> (,4) . ldHD
    0x63 -> (,4) . ldHE
    0x64 -> (,4) . ldHH
    0x65 -> (,4) . ldHL
    0x66 -> (,8) . ldHHL
    0x67 -> (,4) . ldHA
    0x68 -> (,4) . ldLB
    0x69 -> (,4) . ldLC
    0x6A -> (,4) . ldLD
    0x6B -> (,4) . ldLE
    0x6C -> (,4) . ldLH
    0x6D -> (,4) . ldLL
    0x6E -> (,8) . ldLHL
    0x6F -> (,4) . ldLA
    0x70 -> (,8) . ldHLB
    0x71 -> (,8) . ldHLC
    0x72 -> (,8) . ldHLD
    0x73 -> (,8) . ldHLE
    0x74 -> (,8) . ldHLH
    0x75 -> (,8) . ldHLL
    0x76 -> (,4) . halt
    0x77 -> (,8) . ldHLA
    0x78 -> (,4) . ldAB
    0x79 -> (,4) . ldAC
    0x7A -> (,4) . ldAD
    0x7B -> (,4) . ldAE
    0x7C -> (,4) . ldAH
    0x7D -> (,4) . ldAL
    0x7E -> (,8) . ldAHL
    0x7F -> (,4) . ldAA
    0x80 -> (,4) . addAB
    0x81 -> (,4) . addAC
    0x82 -> (,4) . addAD
    0x83 -> (,4) . addAE
    0x84 -> (,4) . addAH
    0x85 -> (,4) . addAL
    0x86 -> (,8) . addAHL
    0x87 -> (,4) . addAA
    0x88 -> (,4) . adcAB
    0x89 -> (,4) . adcAC
    0x8A -> (,4) . adcAD
    0x8B -> (,4) . adcAE
    0x8C -> (,4) . adcAH
    0x8D -> (,4) . adcAL
    0x8E -> (,8) . adcAHL
    0x8F -> (,4) . adcAA
    0x90 -> (,4) . subB
    0x91 -> (,4) . subC
    0x92 -> (,4) . subD
    0x93 -> (,4) . subE
    0x94 -> (,4) . subH
    0x95 -> (,4) . subL
    0x96 -> (,8) . subHL
    0x97 -> (,4) . subA
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

-- TODO: DAA
daa :: CPU -> CPU
daa cpu = undefined

ldBA :: CPU -> CPU
ldBA cpu = cpu & cpuRegisterB .~ (cpu ^. cpuRegisterA)

-- Not sure if it actually needs to be implemented in terms other than nop, but here we go
ldBB :: CPU -> CPU
ldBB cpu = cpu & cpuRegisterB .~ (cpu ^. cpuRegisterB)

ldBC :: CPU -> CPU
ldBC cpu = cpu & cpuRegisterB .~ (cpu ^. cpuRegisterC)

ldBD :: CPU -> CPU
ldBD cpu = cpu & cpuRegisterB .~ (cpu ^. cpuRegisterD)

ldBE :: CPU -> CPU
ldBE cpu = cpu & cpuRegisterB .~ (cpu ^. cpuRegisterE)

ldBH :: CPU -> CPU
ldBH cpu = cpu & cpuRegisterB .~ (cpu ^. cpuRegisterH)

ldBL :: CPU -> CPU
ldBL cpu = cpu & cpuRegisterB .~ (cpu ^. cpuRegisterL)

ldBHL :: CPU -> CPU
ldBHL cpu = cpu & cpuRegisterB .~ mcuLookup (cpu ^. cpuRegisterHL) cpu

ldCA :: CPU -> CPU
ldCA cpu = cpu & cpuRegisterC .~ (cpu ^. cpuRegisterA)

ldCB :: CPU -> CPU
ldCB cpu = cpu & cpuRegisterC .~ (cpu ^. cpuRegisterB)

ldCC :: CPU -> CPU
ldCC cpu = cpu & cpuRegisterC .~ (cpu ^. cpuRegisterC)

ldCD :: CPU -> CPU
ldCD cpu = cpu & cpuRegisterC .~ (cpu ^. cpuRegisterD)

ldCE :: CPU -> CPU
ldCE cpu = cpu & cpuRegisterC .~ (cpu ^. cpuRegisterE)

ldCH :: CPU -> CPU
ldCH cpu = cpu & cpuRegisterC .~ (cpu ^. cpuRegisterH)

ldCL :: CPU -> CPU
ldCL cpu = cpu & cpuRegisterC .~ (cpu ^. cpuRegisterL)

ldCHL :: CPU -> CPU
ldCHL cpu = cpu & cpuRegisterC .~ mcuLookup (cpu ^. cpuRegisterHL) cpu

ldDA :: CPU -> CPU
ldDA cpu = cpu & cpuRegisterD .~ (cpu ^. cpuRegisterA)

ldDB :: CPU -> CPU
ldDB cpu = cpu & cpuRegisterD .~ (cpu ^. cpuRegisterB)

ldDC :: CPU -> CPU
ldDC cpu = cpu & cpuRegisterD .~ (cpu ^. cpuRegisterC)

ldDD :: CPU -> CPU
ldDD cpu = cpu & cpuRegisterD .~ (cpu ^. cpuRegisterD)

ldDE :: CPU -> CPU
ldDE cpu = cpu & cpuRegisterD .~ (cpu ^. cpuRegisterE)

ldDH :: CPU -> CPU
ldDH cpu = cpu & cpuRegisterD .~ (cpu ^. cpuRegisterH)

ldDL :: CPU -> CPU
ldDL cpu = cpu & cpuRegisterD .~ (cpu ^. cpuRegisterL)

ldDHL :: CPU -> CPU
ldDHL cpu = cpu & cpuRegisterD .~ mcuLookup (cpu ^. cpuRegisterHL) cpu

ldEA :: CPU -> CPU
ldEA cpu = cpu & cpuRegisterE .~ (cpu ^. cpuRegisterA)

ldEB :: CPU -> CPU
ldEB cpu = cpu & cpuRegisterE .~ (cpu ^. cpuRegisterB)

ldEC :: CPU -> CPU
ldEC cpu = cpu & cpuRegisterE .~ (cpu ^. cpuRegisterC)

ldED :: CPU -> CPU
ldED cpu = cpu & cpuRegisterE .~ (cpu ^. cpuRegisterD)

ldEE :: CPU -> CPU
ldEE cpu = cpu & cpuRegisterE .~ (cpu ^. cpuRegisterE)

ldEH :: CPU -> CPU
ldEH cpu = cpu & cpuRegisterE .~ (cpu ^. cpuRegisterH)

ldEL :: CPU -> CPU
ldEL cpu = cpu & cpuRegisterE .~ (cpu ^. cpuRegisterL)

ldEHL :: CPU -> CPU
ldEHL cpu = cpu & cpuRegisterE .~ mcuLookup (cpu ^. cpuRegisterHL) cpu

ldHA :: CPU -> CPU
ldHA cpu = cpu & cpuRegisterH .~ (cpu ^. cpuRegisterA)

ldHB :: CPU -> CPU
ldHB cpu = cpu & cpuRegisterH .~ (cpu ^. cpuRegisterB)

ldHC :: CPU -> CPU
ldHC cpu = cpu & cpuRegisterH .~ (cpu ^. cpuRegisterC)

ldHD :: CPU -> CPU
ldHD cpu = cpu & cpuRegisterH .~ (cpu ^. cpuRegisterD)

ldHE :: CPU -> CPU
ldHE cpu = cpu & cpuRegisterH .~ (cpu ^. cpuRegisterE)

ldHH :: CPU -> CPU
ldHH cpu = cpu & cpuRegisterH .~ (cpu ^. cpuRegisterH)

ldHL :: CPU -> CPU
ldHL cpu = cpu & cpuRegisterH .~ (cpu ^. cpuRegisterL)

ldHHL :: CPU -> CPU
ldHHL cpu = cpu & cpuRegisterH .~ mcuLookup (cpu ^. cpuRegisterHL) cpu

ldLA :: CPU -> CPU
ldLA cpu = cpu & cpuRegisterL .~ (cpu ^. cpuRegisterA)

ldLB :: CPU -> CPU
ldLB cpu = cpu & cpuRegisterL .~ (cpu ^. cpuRegisterB)

ldLC :: CPU -> CPU
ldLC cpu = cpu & cpuRegisterL .~ (cpu ^. cpuRegisterC)

ldLD :: CPU -> CPU
ldLD cpu = cpu & cpuRegisterL .~ (cpu ^. cpuRegisterD)

ldLE :: CPU -> CPU
ldLE cpu = cpu & cpuRegisterL .~ (cpu ^. cpuRegisterE)

ldLH :: CPU -> CPU
ldLH cpu = cpu & cpuRegisterL .~ (cpu ^. cpuRegisterH)

ldLL :: CPU -> CPU
ldLL cpu = cpu & cpuRegisterL .~ (cpu ^. cpuRegisterL)

ldLHL :: CPU -> CPU
ldLHL cpu = cpu & cpuRegisterL .~ mcuLookup (cpu ^. cpuRegisterHL) cpu

ldHLA :: CPU -> CPU
ldHLA cpu = mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterA) cpu

ldHLB :: CPU -> CPU
ldHLB cpu = mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterB) cpu

ldHLC :: CPU -> CPU
ldHLC cpu = mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterC) cpu

ldHLD :: CPU -> CPU
ldHLD cpu = mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterD) cpu

ldHLE :: CPU -> CPU
ldHLE cpu = mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterE) cpu

ldHLH :: CPU -> CPU
ldHLH cpu = mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterH) cpu

ldHLL :: CPU -> CPU
ldHLL cpu = mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterL) cpu

ldAA :: CPU -> CPU
ldAA cpu = cpu & cpuRegisterA .~ (cpu ^. cpuRegisterA)

ldAB :: CPU -> CPU
ldAB cpu = cpu & cpuRegisterA .~ (cpu ^. cpuRegisterB)

ldAC :: CPU -> CPU
ldAC cpu = cpu & cpuRegisterA .~ (cpu ^. cpuRegisterC)

ldAD :: CPU -> CPU
ldAD cpu = cpu & cpuRegisterA .~ (cpu ^. cpuRegisterD)

ldAE :: CPU -> CPU
ldAE cpu = cpu & cpuRegisterA .~ (cpu ^. cpuRegisterE)

ldAH :: CPU -> CPU
ldAH cpu = cpu & cpuRegisterA .~ (cpu ^. cpuRegisterH)

ldAL :: CPU -> CPU
ldAL cpu = cpu & cpuRegisterA .~ (cpu ^. cpuRegisterL)

ldAHL :: CPU -> CPU
ldAHL cpu = cpu & cpuRegisterA .~ mcuLookup (cpu ^. cpuRegisterHL) cpu

ldABC :: CPU -> CPU
ldABC cpu = cpu & cpuRegisterA .~ mcuLookup (cpu ^. cpuRegisterBC) cpu

ldADE :: CPU -> CPU
ldADE cpu = cpu & cpuRegisterA .~ mcuLookup (cpu ^. cpuRegisterDE) cpu

ldBCA :: CPU -> CPU
ldBCA cpu = mcuWrite (cpu ^. cpuRegisterBC) (cpu ^. cpuRegisterA) cpu

ldDEA :: CPU -> CPU
ldDEA cpu = mcuWrite (cpu ^. cpuRegisterDE) (cpu ^. cpuRegisterA) cpu

ldHLplusA :: CPU -> CPU
ldHLplusA cpu =
  mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterA) cpu
    & cpuRegisterHL +~ 1

ldHLminusA :: CPU -> CPU
ldHLminusA cpu =
  mcuWrite (cpu ^. cpuRegisterHL) (cpu ^. cpuRegisterA) cpu
    & cpuRegisterHL -~ 1

ldAHLplus :: CPU -> CPU
ldAHLplus cpu =
  cpu & cpuRegisterA .~ mcuLookup (cpu ^. cpuRegisterHL) cpu
    & cpuRegisterHL +~ 1

ldAHLminus :: CPU -> CPU
ldAHLminus cpu =
  cpu & cpuRegisterA .~ mcuLookup (cpu ^. cpuRegisterHL) cpu
    & cpuRegisterHL -~ 1

ldd8 :: Lens' CPU Word8 -> CPU -> CPU
ldd8 reg cpu = cpu' & reg .~ d8
  where
    (cpu', d8) = pcLookup cpu

ldBd8 :: CPU -> CPU
ldBd8 = ldd8 cpuRegisterB

ldCd8 :: CPU -> CPU
ldCd8 = ldd8 cpuRegisterC

ldDd8 :: CPU -> CPU
ldDd8 = ldd8 cpuRegisterD

ldEd8 :: CPU -> CPU
ldEd8 = ldd8 cpuRegisterE

ldHd8 :: CPU -> CPU
ldHd8 = ldd8 cpuRegisterH

ldLd8 :: CPU -> CPU
ldLd8 = ldd8 cpuRegisterL

ldAd8 :: CPU -> CPU
ldAd8 = ldd8 cpuRegisterA

ldHLd8 :: CPU -> CPU
ldHLd8 cpu = mcuWrite (cpu' ^. cpuRegisterHL) d8 cpu'
  where
    (cpu', d8) = pcLookup cpu

ldd16 :: Lens' CPU Word16 -> CPU -> CPU
ldd16 reg cpu = cpu'' & reg .~ mkWord16 msb lsb
  where
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

ldBCd16 :: CPU -> CPU
ldBCd16 = ldd16 cpuRegisterBC

ldDEd16 :: CPU -> CPU
ldDEd16 = ldd16 cpuRegisterDE

ldHLd16 :: CPU -> CPU
ldHLd16 = ldd16 cpuRegisterHL

ldSPd16 :: CPU -> CPU
ldSPd16 = ldd16 cpuSP

lda16SP :: CPU -> CPU
lda16SP cpu = mcuWrite (mkWord16 msb lsb + 1) spmsb . mcuWrite (mkWord16 msb lsb) splsb $ cpu''
  where
    (spmsb, splsb) = splitWord16 (cpu ^. cpuSP)
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

lda16A :: CPU -> CPU
lda16A cpu = mcuWrite (mkWord16 msb lsb) (cpu ^. cpuRegisterA) cpu''
  where
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

ldAa16 :: CPU -> CPU
ldAa16 cpu = cpu'' & cpuRegisterA .~ mcuLookup (mkWord16 msb lsb) cpu''
  where
    (cpu', lsb) = pcLookup cpu
    (cpu'', msb) = pcLookup cpu'

ldSPHL :: CPU -> CPU
ldSPHL cpu = cpu & cpuSP .~ (cpu ^. cpuRegisterHL)

incBC :: CPU -> CPU
incBC cpu = cpu & cpuRegisterBC +~ 1

incDE :: CPU -> CPU
incDE cpu = cpu & cpuRegisterDE +~ 1

incHL :: CPU -> CPU
incHL cpu = cpu & cpuRegisterHL +~ 1

incSP :: CPU -> CPU
incSP cpu = cpu & cpuSP +~ 1

decBC :: CPU -> CPU
decBC cpu = cpu & cpuRegisterBC -~ 1

decDE :: CPU -> CPU
decDE cpu = cpu & cpuRegisterDE -~ 1

decHL :: CPU -> CPU
decHL cpu = cpu & cpuRegisterHL -~ 1

decSP :: CPU -> CPU
decSP cpu = cpu & cpuSP -~ 1

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

incB :: CPU -> CPU
incB = inc cpuRegisterB

incC :: CPU -> CPU
incC = inc cpuRegisterC

incD :: CPU -> CPU
incD = inc cpuRegisterD

incE :: CPU -> CPU
incE = inc cpuRegisterE

incH :: CPU -> CPU
incH = inc cpuRegisterH

incL :: CPU -> CPU
incL = inc cpuRegisterL

incA :: CPU -> CPU
incA = inc cpuRegisterA

incHL_ :: CPU -> CPU
incHL_ cpu =
  mcuWrite (cpu ^. address) (val + 1) cpu
    & cpuFlagH .~ (0x0F .&. val + 1 > 0x0F)
    & cpuFlagZ .~ (val + 1 == 0x00)
    & cpuFlagN .~ False
  where
    val = mcuLookup (cpu ^. address) cpu
    address = cpuRegisterHL

dec :: Lens' CPU Word8 -> CPU -> CPU
dec reg cpu =
  cpu & reg -~ 1
    & cpuFlagH .~ (0x0F .&. cpu ^. reg == 0x00)
    & cpuFlagZ .~ (cpu ^. reg - 1 == 0x00)
    & cpuFlagN .~ True

decB :: CPU -> CPU
decB = dec cpuRegisterB

decC :: CPU -> CPU
decC = dec cpuRegisterC

decD :: CPU -> CPU
decD = dec cpuRegisterD

decE :: CPU -> CPU
decE = dec cpuRegisterE

decH :: CPU -> CPU
decH = dec cpuRegisterH

decL :: CPU -> CPU
decL = dec cpuRegisterL

decA :: CPU -> CPU
decA = dec cpuRegisterA

decHL_ :: CPU -> CPU
decHL_ cpu =
  mcuWrite (cpu ^. address) (val - 1) cpu
    & cpuFlagH .~ (0x0F .&. val == 0x00)
    & cpuFlagZ .~ (val - 1 == 0x00)
    & cpuFlagN .~ True
  where
    val = mcuLookup (cpu ^. address) cpu
    address = cpuRegisterHL

addHL :: Lens' CPU Word16 -> CPU -> CPU
addHL reg cpu =
  cpu & cpuRegisterHL .~ op1 + op2
    & cpuFlagC .~ (op1 + op2 < op1)
    & cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
    & cpuFlagN .~ False
  where
    op1 = cpu ^. cpuRegisterHL
    op2 = cpu ^. reg

addHLBC :: CPU -> CPU
addHLBC = addHL cpuRegisterBC

addHLDE :: CPU -> CPU
addHLDE = addHL cpuRegisterDE

addHLHL :: CPU -> CPU
addHLHL = addHL cpuRegisterHL

addHLSP :: CPU -> CPU
addHLSP = addHL cpuSP

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

addAB :: CPU -> CPU
addAB = addA cpuRegisterB

addAC :: CPU -> CPU
addAC = addA cpuRegisterC

addAD :: CPU -> CPU
addAD = addA cpuRegisterD

addAE :: CPU -> CPU
addAE = addA cpuRegisterE

addAH :: CPU -> CPU
addAH = addA cpuRegisterH

addAL :: CPU -> CPU
addAL = addA cpuRegisterL

addAHL :: CPU -> CPU
addAHL cpu = addA (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

addAA :: CPU -> CPU
addAA = addA cpuRegisterA

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

adcAB :: CPU -> CPU
adcAB = adcA cpuRegisterB

adcAC :: CPU -> CPU
adcAC = adcA cpuRegisterC

adcAD :: CPU -> CPU
adcAD = adcA cpuRegisterD

adcAE :: CPU -> CPU
adcAE = adcA cpuRegisterE

adcAH :: CPU -> CPU
adcAH = adcA cpuRegisterH

adcAL :: CPU -> CPU
adcAL = adcA cpuRegisterL

adcAHL :: CPU -> CPU
adcAHL cpu = adcA (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

adcAA :: CPU -> CPU
adcAA = adcA cpuRegisterA

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

subB :: CPU -> CPU
subB = sub cpuRegisterB

subC :: CPU -> CPU
subC = sub cpuRegisterC

subD :: CPU -> CPU
subD = sub cpuRegisterD

subE :: CPU -> CPU
subE = sub cpuRegisterE

subH :: CPU -> CPU
subH = sub cpuRegisterH

subL :: CPU -> CPU
subL = sub cpuRegisterL

subHL :: CPU -> CPU
subHL cpu = sub (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

subA :: CPU -> CPU
subA = sub cpuRegisterA

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
sbcHL cpu = sbc (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

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
andHL cpu = aAnd (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

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
xorHL cpu = aXor (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

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
orHL cpu = aor (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

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
cpHL cpu = acp (cpuRegisterHL . to (`mcuLookup` cpu)) cpu

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
    lsb = mcuLookup (cpu ^. cpuSP) cpu
    msb = mcuLookup (cpu ^. cpuSP + 1) cpu

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
    lsb = mcuLookup (cpu ^. cpuSP) cpu
    msb = mcuLookup (cpu ^. cpuSP + 1) cpu

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
  cpu & mcuWrite (cpu ^. cpuSP - 1) msb
    & mcuWrite (cpu ^. cpuSP - 2) lsb
    & cpuSP -~ 2
  where
    (msb, lsb) = splitWord16 $ cpu ^. reg

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
  cpu'' & mcuWrite (cpu ^. cpuSP - 1) pcmsb
    & mcuWrite (cpu ^. cpuSP - 2) pclsb
    & cpuSP -~ 2
    & cpuPC .~ mkWord16 targetmsb targetlsb
  where
    (pcmsb, pclsb) = splitWord16 $ cpu ^. cpuPC
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
  cpu & mcuWrite (cpu ^. cpuSP - 1) pcmsb
    & mcuWrite (cpu ^. cpuSP - 2) pclsb
    & cpuSP -~ 2
    & cpuPC .~ mkWord16 0x00 lsb
  where
    (pcmsb, pclsb) = splitWord16 $ cpu ^. cpuPC

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
ldhCA cpu = mcuWrite (mkWord16 0xFF lsb) (cpu ^. cpuRegisterA) cpu
  where
    lsb = cpu ^. cpuRegisterC

ldhAC :: CPU -> CPU
ldhAC cpu = cpu & cpuRegisterA .~ newVal
  where
    newVal = mcuLookup (mkWord16 0xFF lsb) cpu
    lsb = cpu ^. cpuRegisterC

ldha8A :: CPU -> CPU
ldha8A cpu = cpu' & mcuWrite (mkWord16 0xFF lsb) (cpu' ^. cpuRegisterA)
  where
    (cpu', lsb) = pcLookup cpu

ldhAa8 :: CPU -> CPU
ldhAa8 cpu = cpu' & cpuRegisterA .~ newVal
  where
    newVal = mcuLookup (mkWord16 0xFF lsb) cpu'
    (cpu', lsb) = pcLookup cpu
