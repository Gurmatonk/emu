{-# LANGUAGE RankNTypes #-}

module CPU where

import Control.Lens
import Data.Bits (bit, complement, rotateL, rotateR, xor, (.&.), (.|.))
import Data.Bool (bool)
import Data.Int (Int8)
import Data.Tuple (swap)
import Data.Word (Word8, Word16)

import Types

import qualified MCU

import CB (cb)
import Lenses (cpuFlagC, cpuFlagH, cpuFlagN, cpuFlagZ, cpuRegisterAF, cpuRegisterBC, cpuRegisterDE, cpuRegisterHL, mcuLookup, mcuWrite, pcLookup)
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

runInstruction :: CPU -> CPU
runInstruction = uncurry execInstruction . swap . pcLookup

execInstruction :: Word8 -> CPU -> CPU
execInstruction opcode =
  case opcode of
    0x00 -> nop
    0x01 -> ldBCd16
    0x02 -> ldBCA
    0x03 -> incBC
    0x04 -> incB
    0x05 -> decB
    0x06 -> ldBd8
    0x07 -> rlcA
    0x08 -> lda16SP
    0x09 -> addHLBC
    0x0A -> ldABC
    0x0B -> decBC
    0x0C -> incC
    0x0D -> decC
    0x0E -> ldCd8
    0x0F -> rrcA
    0x10 -> stop
    0x11 -> ldDEd16
    0x12 -> ldDEA
    0x13 -> incDE
    0x14 -> incD
    0x15 -> decD
    0x16 -> ldDd8
    0x17 -> rlA
    0x18 -> jrr8
    0x19 -> addHLDE
    0x1A -> ldADE
    0x1B -> decDE
    0x1C -> incE
    0x1D -> decE
    0x1E -> ldEd8
    0x1F -> rrA
    0x20 -> jrNZr8
    0x21 -> ldHLd16
    0x22 -> ldHLplusA
    0x23 -> incHL
    0x24 -> incH
    0x25 -> decH
    0x26 -> ldHd8
    0x27 -> daa
    0x28 -> jrZr8
    0x29 -> addHLHL
    0x2A -> ldAHLplus
    0x2B -> decHL
    0x2C -> incL
    0x2D -> decL
    0x2E -> ldLd8
    0x2F -> cpl
    0x30 -> jrNCr8
    0x31 -> ldSPd16
    0x32 -> ldHLminusA
    0x33 -> incSP
    0x34 -> incHL_
    0x35 -> decHL_
    0x36 -> ldHLd8
    0x37 -> scf
    0x38 -> jrCr8
    0x39 -> addHLSP
    0x3A -> ldAHLminus
    0x3B -> decSP
    0x3C -> incA
    0x3D -> decA
    0x3E -> ldAd8
    0x3F -> ccf
    0x40 -> ldBB
    0x41 -> ldBC
    0x42 -> ldBD
    0x43 -> ldBE
    0x44 -> ldBH
    0x45 -> ldBL
    0x46 -> ldBHL
    0x47 -> ldBA
    0x48 -> ldCB
    0x49 -> ldCC
    0x4A -> ldCD
    0x4B -> ldCE
    0x4C -> ldCH
    0x4D -> ldCL
    0x4E -> ldCHL
    0x4F -> ldCA
    0x50 -> ldDB
    0x51 -> ldDC
    0x52 -> ldDD
    0x53 -> ldDE
    0x54 -> ldDH
    0x55 -> ldDL
    0x56 -> ldDHL
    0x57 -> ldDA
    0x58 -> ldEB
    0x59 -> ldEC
    0x5A -> ldED
    0x5B -> ldEE
    0x5C -> ldEH
    0x5D -> ldEL
    0x5E -> ldEHL
    0x5F -> ldEA
    0x60 -> ldHB
    0x61 -> ldHC
    0x62 -> ldHD
    0x63 -> ldHE
    0x64 -> ldHH
    0x65 -> ldHL
    0x66 -> ldHHL
    0x67 -> ldHA
    0x68 -> ldLB
    0x69 -> ldLC
    0x6A -> ldLD
    0x6B -> ldLE
    0x6C -> ldLH
    0x6D -> ldLL
    0x6E -> ldLHL
    0x6F -> ldLA
    0x70 -> ldHLB
    0x71 -> ldHLC
    0x72 -> ldHLD
    0x73 -> ldHLE
    0x74 -> ldHLH
    0x75 -> ldHLL
    0x76 -> halt
    0x77 -> ldHLA
    0x78 -> ldAB
    0x79 -> ldAC
    0x7A -> ldAD
    0x7B -> ldAE
    0x7C -> ldAH
    0x7D -> ldAL
    0x7E -> ldAHL
    0x7F -> ldAA
    0x80 -> addAB
    0x81 -> addAC
    0x82 -> addAD
    0x83 -> addAE
    0x84 -> addAH
    0x85 -> addAL
    0x86 -> addAHL
    0x87 -> addAA
    0x88 -> adcAB
    0x89 -> adcAC
    0x8A -> adcAD
    0x8B -> adcAE
    0x8C -> adcAH
    0x8D -> adcAL
    0x8E -> adcAHL
    0x8F -> adcAA
    0x90 -> subB
    0x91 -> subC
    0x92 -> subD
    0x93 -> subE
    0x94 -> subH
    0x95 -> subL
    0x96 -> subHL
    0x97 -> subA
    0x98 -> sbcB
    0x99 -> sbcC
    0x9A -> sbcD
    0x9B -> sbcE
    0x9C -> sbcH
    0x9D -> sbcL
    0x9E -> sbcHL
    0x9F -> sbcA
    0xA0 -> andB
    0xA1 -> andC
    0xA2 -> andD
    0xA3 -> andE
    0xA4 -> andH
    0xA5 -> andL
    0xA6 -> andHL
    0xA7 -> andA
    0xA8 -> xorB
    0xA9 -> xorC
    0xAA -> xorD
    0xAB -> xorE
    0xAC -> xorH
    0xAD -> xorL
    0xAE -> xorHL
    0xAF -> xorA
    0xB0 -> orB
    0xB1 -> orC
    0xB2 -> orD
    0xB3 -> orE
    0xB4 -> orH
    0xB5 -> orL
    0xB6 -> orHL
    0xB7 -> orA
    0xB8 -> cpB
    0xB9 -> cpC
    0xBA -> cpD
    0xBB -> cpE
    0xBC -> cpH
    0xBD -> cpL
    0xBE -> cpHL
    0xBF -> cpA
    0xC0 -> retNZ
    0xC1 -> popBC
    0xC2 -> jpNZa16
    0xC3 -> jpa16
    0xC4 -> callNZa16
    0xC5 -> pushBC
    0xC6 -> addAd8
    0xC7 -> rst00
    0xC8 -> retZ
    0xC9 -> ret
    0xCA -> jpZa16
    0xCB -> cb
    0xCC -> callZa16
    0xCD -> calla16
    0xCE -> adcAd8
    0xCF -> rst08
    0xD0 -> retNC
    0xD1 -> popDE
    0xD2 -> jpNCa16
    0xD4 -> callNCa16
    0xD5 -> pushDE
    0xD6 -> subd8
    0xD7 -> rst10
    0xD8 -> retC
    0xD9 -> reti
    0xDA -> jpCa16
    0xDC -> callCa16
    0xDE -> sbcAd8
    0xDF -> rst18
    0xE0 -> ldha8A
    0xE1 -> popHL
    0xE2 -> ldhCA
    0xE5 -> pushHL
    0xE6 -> andd8
    0xE7 -> rst20
    0xE8 -> addSPr8
    0xE9 -> jpHL
    0xEA -> lda16A
    0xEE -> xord8
    0xEF -> rst28
    0xF0 -> ldhAa8
    0xF1 -> popAF
    0xF2 -> ldhAC
    0xF3 -> di
    0xF5 -> pushAF
    0xF6 -> ord8
    0xF7 -> rst30
    0xF8 -> ldHLSPplusr8
    0xF9 -> ldSPHL
    0xFA -> ldAa16
    0xFB -> ei
    0xFE -> cpd8
    0xFF -> rst38
    _ -> undefined

nop :: CPU -> CPU
nop = id

-- TODO: Halt until interrupt occurs!
halt :: CPU -> CPU
halt cpu = undefined

-- TODO: Low power standby - whatever that is
-- NOTE: From the specs this should encode as 0x1000, i.e. 2 bytes - since this is unnecessary it is 'sometimes' encoded simply as 0x10
--       - might need to check whether PC should be incremented by 1 or 2....
stop :: CPU -> CPU
stop cpu = undefined & cpuPC +~ 1 -- TODO: Extra inc according to spec. Delete, maybe...

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
  (
    if r8 >= 0
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
  (
    if r8 >= 0
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
  cpu' & ( if r8 < 0
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