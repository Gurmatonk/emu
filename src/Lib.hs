{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens
import Data.Bits (bit, complement, rotateL, rotateR, setBit, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Bool (bool)
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
import Data.Word
import Data.Tuple (swap)

data Gameboy = Gameboy
  { _gbCPU :: CPU,
    _gbRAM :: RAM,
    _gbIME :: Bool
  }
  deriving (Show)

type RAM = Map Word16 Word8

data CPU = CPU
  { _cpuRegisterA :: Word8,
    _cpuRegisterF :: Word8,
    _cpuRegisterB :: Word8,
    _cpuRegisterC :: Word8,
    _cpuRegisterD :: Word8,
    _cpuRegisterE :: Word8,
    _cpuRegisterH :: Word8,
    _cpuRegisterL :: Word8,
    _cpuSP :: Word16,
    _cpuPC :: Word16
  }
  deriving (Show)

makeLenses ''Gameboy
makeLenses ''CPU

-- flipping single bits like it's 1989 :)
bitwiseValue :: Word8 -> Lens' Word8 Bool
bitwiseValue mask = lens getter setter
  where
    getter w = (mask .&. w) > 0
    setter w = bool (w .&. complement mask) (w .|. mask)

cpuFlagZ :: Lens' CPU Bool
cpuFlagZ = cpuRegisterF . bitwiseValue (bit 7)

cpuFlagN :: Lens' CPU Bool
cpuFlagN = cpuRegisterF . bitwiseValue (bit 6)

cpuFlagH :: Lens' CPU Bool
cpuFlagH = cpuRegisterF . bitwiseValue (bit 5)

cpuFlagC :: Lens' CPU Bool
cpuFlagC = cpuRegisterF . bitwiseValue (bit 4)

combinedRegister :: Lens' CPU Word8 -> Lens' CPU Word8 -> Lens' CPU Word16
combinedRegister hi lo = lens getter setter
  where
    getter cpu = mkWord16 (cpu ^. hi) (cpu ^. lo)
    setter cpu w = 
      cpu & hi .~ (fst . splitWord16 $ w)
        & lo .~ (snd . splitWord16 $ w)

cpuRegisterHL :: Lens' CPU Word16
cpuRegisterHL = combinedRegister cpuRegisterH cpuRegisterL

cpuRegisterBC :: Lens' CPU Word16
cpuRegisterBC = combinedRegister cpuRegisterB cpuRegisterC

cpuRegisterDE :: Lens' CPU Word16
cpuRegisterDE = combinedRegister cpuRegisterD cpuRegisterE

-- only used when popping/pushing the stack pointer
cpuRegisterAF :: Lens' CPU Word16
cpuRegisterAF = combinedRegister cpuRegisterA cpuRegisterF

initGameboy :: Gameboy
initGameboy =
  Gameboy
    { _gbCPU = initCpu,
      _gbRAM = mempty,
      _gbIME = False
    }

initCpu :: CPU
initCpu =
  CPU
    { _cpuRegisterA = 0x01,
      _cpuRegisterF = 0xB0,
      _cpuRegisterB = 0xFE,
      _cpuRegisterC = 0x13,
      _cpuRegisterD = 0x00,
      _cpuRegisterE = 0xD8,
      _cpuRegisterH = 0x01,
      _cpuRegisterL = 0x4D,
      _cpuSP = 0xFFFE,
      _cpuPC = 0x0100
    }

mkWord16 ::
  -- | HI
  Word8 ->
  -- | LO
  Word8 ->
  Word16
mkWord16 hi lo = ((fromIntegral hi :: Word16) `shiftL` 8) + fromIntegral lo

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = (fromIntegral ((w .&. 0xFF00) `shiftR` 8), fromIntegral (w .&. 0x00FF))

-- s -> (s, a) is actually State... consider!
pcLookup :: Gameboy -> (Gameboy, Word8)
pcLookup gb = (gb & gbCPU . cpuPC +~ 1, res)
  where
    res =
      case M.lookup (gb ^. gbCPU . cpuPC) (gb ^. gbRAM) of
        (Just v) -> v
        Nothing -> 0xFF

ramLookup' :: Word16 -> RAM -> Word8
ramLookup' l ram =
  case M.lookup l ram of
    (Just v) -> v
    Nothing -> 0xFF -- TODO: Check if good default

writeToRam :: Word16 -> Word8 -> Gameboy -> Gameboy
writeToRam k v gb = gb & gbRAM . at k ?~ v

runInstruction :: Gameboy -> Gameboy
runInstruction = uncurry execInstruction . swap . pcLookup

execInstruction :: Word8 -> Gameboy -> Gameboy
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

cb :: Gameboy -> Gameboy
cb = uncurry execcb . swap . pcLookup

execcb :: Word8 -> Gameboy -> Gameboy
execcb opcode =
  case opcode of
    0x80 -> res0B
    _ -> undefined

res0B :: Gameboy -> Gameboy
res0B = gbCPU . cpuRegisterB . bitwiseValue (bit 0) .~ False

nop :: Gameboy -> Gameboy
nop = id

-- TODO: Halt until interrupt occurs!
halt :: Gameboy -> Gameboy
halt gb = undefined

-- TODO: Low power standby - whatever that is
-- NOTE: From the specs this should encode as 0x1000, i.e. 2 bytes - since this is unnecessary it is 'sometimes' encoded simply as 0x10
--       - might need to check whether PC should be incremented by 1 or 2....
stop :: Gameboy -> Gameboy
stop gb = undefined & gbCPU . cpuPC +~ 1 -- TODO: Extra inc according to spec. Delete, maybe...

-- TODO: DAA
daa :: Gameboy -> Gameboy
daa gb = undefined

ldBA :: Gameboy -> Gameboy
ldBA gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterA)

-- Not sure if it actually needs to be implemented in terms other than nop, but here we go
ldBB :: Gameboy -> Gameboy
ldBB gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterB)

ldBC :: Gameboy -> Gameboy
ldBC gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterC)

ldBD :: Gameboy -> Gameboy
ldBD gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterD)

ldBE :: Gameboy -> Gameboy
ldBE gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterE)

ldBH :: Gameboy -> Gameboy
ldBH gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterH)

ldBL :: Gameboy -> Gameboy
ldBL gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterL)

ldBHL :: Gameboy -> Gameboy
ldBHL gb = gb & gbCPU . cpuRegisterB .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)

ldCA :: Gameboy -> Gameboy
ldCA gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterA)

ldCB :: Gameboy -> Gameboy
ldCB gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterB)

ldCC :: Gameboy -> Gameboy
ldCC gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterC)

ldCD :: Gameboy -> Gameboy
ldCD gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterD)

ldCE :: Gameboy -> Gameboy
ldCE gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterE)

ldCH :: Gameboy -> Gameboy
ldCH gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterH)

ldCL :: Gameboy -> Gameboy
ldCL gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterL)

ldCHL :: Gameboy -> Gameboy
ldCHL gb = gb & gbCPU . cpuRegisterC .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)

ldDA :: Gameboy -> Gameboy
ldDA gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterA)

ldDB :: Gameboy -> Gameboy
ldDB gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterB)

ldDC :: Gameboy -> Gameboy
ldDC gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterC)

ldDD :: Gameboy -> Gameboy
ldDD gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterD)

ldDE :: Gameboy -> Gameboy
ldDE gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterE)

ldDH :: Gameboy -> Gameboy
ldDH gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterH)

ldDL :: Gameboy -> Gameboy
ldDL gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterL)

ldDHL :: Gameboy -> Gameboy
ldDHL gb = gb & gbCPU . cpuRegisterD .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)

ldEA :: Gameboy -> Gameboy
ldEA gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterA)

ldEB :: Gameboy -> Gameboy
ldEB gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterB)

ldEC :: Gameboy -> Gameboy
ldEC gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterC)

ldED :: Gameboy -> Gameboy
ldED gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterD)

ldEE :: Gameboy -> Gameboy
ldEE gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterE)

ldEH :: Gameboy -> Gameboy
ldEH gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterH)

ldEL :: Gameboy -> Gameboy
ldEL gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterL)

ldEHL :: Gameboy -> Gameboy
ldEHL gb = gb & gbCPU . cpuRegisterE .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)

ldHA :: Gameboy -> Gameboy
ldHA gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterA)

ldHB :: Gameboy -> Gameboy
ldHB gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterB)

ldHC :: Gameboy -> Gameboy
ldHC gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterC)

ldHD :: Gameboy -> Gameboy
ldHD gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterD)

ldHE :: Gameboy -> Gameboy
ldHE gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterE)

ldHH :: Gameboy -> Gameboy
ldHH gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterH)

ldHL :: Gameboy -> Gameboy
ldHL gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterL)

ldHHL :: Gameboy -> Gameboy
ldHHL gb = gb & gbCPU . cpuRegisterH .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)

ldLA :: Gameboy -> Gameboy
ldLA gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterA)

ldLB :: Gameboy -> Gameboy
ldLB gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterB)

ldLC :: Gameboy -> Gameboy
ldLC gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterC)

ldLD :: Gameboy -> Gameboy
ldLD gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterD)

ldLE :: Gameboy -> Gameboy
ldLE gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterE)

ldLH :: Gameboy -> Gameboy
ldLH gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterH)

ldLL :: Gameboy -> Gameboy
ldLL gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterL)

ldLHL :: Gameboy -> Gameboy
ldLHL gb = gb & gbCPU . cpuRegisterL .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)

ldHLA :: Gameboy -> Gameboy
ldHLA gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterA) gb

ldHLB :: Gameboy -> Gameboy
ldHLB gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterB) gb

ldHLC :: Gameboy -> Gameboy
ldHLC gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterC) gb

ldHLD :: Gameboy -> Gameboy
ldHLD gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterD) gb

ldHLE :: Gameboy -> Gameboy
ldHLE gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterE) gb

ldHLH :: Gameboy -> Gameboy
ldHLH gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterH) gb

ldHLL :: Gameboy -> Gameboy
ldHLL gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterL) gb

ldAA :: Gameboy -> Gameboy
ldAA gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterA)

ldAB :: Gameboy -> Gameboy
ldAB gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterB)

ldAC :: Gameboy -> Gameboy
ldAC gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterC)

ldAD :: Gameboy -> Gameboy
ldAD gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterD)

ldAE :: Gameboy -> Gameboy
ldAE gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterE)

ldAH :: Gameboy -> Gameboy
ldAH gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterH)

ldAL :: Gameboy -> Gameboy
ldAL gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterL)

ldAHL :: Gameboy -> Gameboy
ldAHL gb = gb & gbCPU . cpuRegisterA .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)

ldABC :: Gameboy -> Gameboy
ldABC gb = gb & gbCPU . cpuRegisterA .~ ramLookup' (gb ^. gbCPU . cpuRegisterBC) (gb ^. gbRAM)

ldADE :: Gameboy -> Gameboy
ldADE gb = gb & gbCPU . cpuRegisterA .~ ramLookup' (gb ^. gbCPU . cpuRegisterDE) (gb ^. gbRAM)

ldBCA :: Gameboy -> Gameboy
ldBCA gb = writeToRam (gb ^. gbCPU . cpuRegisterBC) (gb ^. gbCPU . cpuRegisterA) gb

ldDEA :: Gameboy -> Gameboy
ldDEA gb = writeToRam (gb ^. gbCPU . cpuRegisterDE) (gb ^. gbCPU . cpuRegisterA) gb

ldHLplusA :: Gameboy -> Gameboy
ldHLplusA gb =
  writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterA) gb
    & gbCPU . cpuRegisterHL +~ 1

ldHLminusA :: Gameboy -> Gameboy
ldHLminusA gb =
  writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterA) gb
    & gbCPU . cpuRegisterHL -~ 1

ldAHLplus :: Gameboy -> Gameboy
ldAHLplus gb =
  gb & gbCPU . cpuRegisterA .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)
    & gbCPU . cpuRegisterHL +~ 1

ldAHLminus :: Gameboy -> Gameboy
ldAHLminus gb =
  gb & gbCPU . cpuRegisterA .~ ramLookup' (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)
    & gbCPU . cpuRegisterHL -~ 1

ldd8 :: Lens' CPU Word8 -> Gameboy -> Gameboy
ldd8 reg gb = gb' & gbCPU . reg .~ d8
  where
    (gb', d8) = pcLookup gb

ldBd8 :: Gameboy -> Gameboy
ldBd8 = ldd8 cpuRegisterB

ldCd8 :: Gameboy -> Gameboy
ldCd8 = ldd8 cpuRegisterC

ldDd8 :: Gameboy -> Gameboy
ldDd8 = ldd8 cpuRegisterD

ldEd8 :: Gameboy -> Gameboy
ldEd8 = ldd8 cpuRegisterE

ldHd8 :: Gameboy -> Gameboy
ldHd8 = ldd8 cpuRegisterH

ldLd8 :: Gameboy -> Gameboy
ldLd8 = ldd8 cpuRegisterL

ldAd8 :: Gameboy -> Gameboy
ldAd8 = ldd8 cpuRegisterA

ldHLd8 :: Gameboy -> Gameboy
ldHLd8 gb = gb' & writeToRam (gb ^. gbCPU . cpuRegisterHL) d8
  where
    (gb', d8) = pcLookup gb

ldd16 :: Lens' CPU Word16 -> Gameboy -> Gameboy
ldd16 reg gb = gb'' & gbCPU . reg .~ mkWord16 msb lsb
  where
    (gb', lsb) = pcLookup gb
    (gb'', msb) = pcLookup gb'

ldBCd16 :: Gameboy -> Gameboy
ldBCd16 = ldd16 cpuRegisterBC

ldDEd16 :: Gameboy -> Gameboy
ldDEd16 = ldd16 cpuRegisterDE

ldHLd16 :: Gameboy -> Gameboy
ldHLd16 = ldd16 cpuRegisterHL

ldSPd16 :: Gameboy -> Gameboy
ldSPd16 = ldd16 cpuSP

lda16SP :: Gameboy -> Gameboy
lda16SP gb =
  gb'' & writeToRam (mkWord16 msb lsb) splsb
    & writeToRam (mkWord16 msb lsb + 1) spmsb
  where
    (spmsb, splsb) = splitWord16 (gb ^. gbCPU . cpuSP)
    (gb', lsb) = pcLookup gb
    (gb'', msb) = pcLookup gb'

lda16A :: Gameboy -> Gameboy
lda16A gb = gb'' & writeToRam (mkWord16 msb lsb) (gb ^. gbCPU . cpuRegisterA)
  where
    (gb', lsb) = pcLookup gb
    (gb'', msb) = pcLookup gb'

ldAa16 :: Gameboy -> Gameboy
ldAa16 gb = gb'' & gbCPU . cpuRegisterA .~ ramLookup' (mkWord16 msb lsb) (gb ^. gbRAM)
  where
    (gb', lsb) = pcLookup gb
    (gb'', msb) = pcLookup gb'

ldSPHL :: Gameboy -> Gameboy
ldSPHL gb = gb & gbCPU . cpuSP .~ (gb ^. gbCPU . cpuRegisterHL)

incBC :: Gameboy -> Gameboy
incBC gb = gb & gbCPU . cpuRegisterBC +~ 1

incDE :: Gameboy -> Gameboy
incDE gb = gb & gbCPU . cpuRegisterDE +~ 1

incHL :: Gameboy -> Gameboy
incHL gb = gb & gbCPU . cpuRegisterHL +~ 1

incSP :: Gameboy -> Gameboy
incSP gb = gb & gbCPU . cpuSP +~ 1

decBC :: Gameboy -> Gameboy
decBC gb = gb & gbCPU . cpuRegisterBC -~ 1

decDE :: Gameboy -> Gameboy
decDE gb = gb & gbCPU . cpuRegisterDE -~ 1

decHL :: Gameboy -> Gameboy
decHL gb = gb & gbCPU . cpuRegisterHL -~ 1

decSP :: Gameboy -> Gameboy
decSP gb = gb & gbCPU . cpuSP -~ 1

rlA :: Gameboy -> Gameboy
rlA gb =
  gb & gbCPU . cpuRegisterA . bitwiseValue (bit 7) .~ new7th -- swap 7th bit with carry so we can ignore carry when rotating
    & gbCPU . cpuFlagC .~ old7th
    & gbCPU . cpuRegisterA %~ (`rotateL` 1)
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    old7th = gb ^. gbCPU . cpuRegisterA . bitwiseValue (bit 7)
    new7th = gb ^. gbCPU . cpuFlagC

rrA :: Gameboy -> Gameboy
rrA gb =
  gb & gbCPU . cpuRegisterA . bitwiseValue (bit 0) .~ new0th -- swap 0th bit with carry so we can ignore carry when rotating
    & gbCPU . cpuFlagC .~ old0th
    & gbCPU . cpuRegisterA %~ (`rotateR` 1)
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    old0th = gb ^. gbCPU . cpuRegisterA . bitwiseValue (bit 0)
    new0th = gb ^. gbCPU . cpuFlagC

-- NOTE: From what I currently understand, RxCA operations only SET the carry to the bit carried over, but DO NOT rotate through it,
--       i.e. the old carry value is overwritten and gone
rlcA :: Gameboy -> Gameboy
rlcA gb =
  gb & gbCPU . cpuFlagC .~ old7th
    & gbCPU . cpuRegisterA %~ (`rotateL` 1)
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    old7th = gb ^. gbCPU . cpuRegisterA . bitwiseValue (bit 7)

rrcA :: Gameboy -> Gameboy
rrcA gb =
  gb & gbCPU . cpuFlagC .~ old0th
    & gbCPU . cpuRegisterA %~ (`rotateR` 1)
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
  where
    old0th = gb ^. gbCPU . cpuRegisterA . bitwiseValue (bit 0)

inc :: Lens' CPU Word8 -> Gameboy -> Gameboy
inc reg gb =
  gb & gbCPU . reg +~ 1
    & gbCPU . cpuFlagH .~ (0x0F .&. gb ^. gbCPU . reg + 1 > 0x0F)
    & gbCPU . cpuFlagZ .~ (gb ^. gbCPU . reg + 1 == 0x00)
    & gbCPU . cpuFlagN .~ False

incB :: Gameboy -> Gameboy
incB = inc cpuRegisterB

incC :: Gameboy -> Gameboy
incC = inc cpuRegisterC

incD :: Gameboy -> Gameboy
incD = inc cpuRegisterD

incE :: Gameboy -> Gameboy
incE = inc cpuRegisterE

incH :: Gameboy -> Gameboy
incH = inc cpuRegisterH

incL :: Gameboy -> Gameboy
incL = inc cpuRegisterL

incA :: Gameboy -> Gameboy
incA = inc cpuRegisterA

incHL_ :: Gameboy -> Gameboy
incHL_ gb =
  gb & writeToRam (gb ^. address) (val + 1)
    & gbCPU . cpuFlagH .~ (0x0F .&. val + 1 > 0x0F)
    & gbCPU . cpuFlagZ .~ (val + 1 == 0x00)
    & gbCPU . cpuFlagN .~ False
  where
    val = ramLookup' (gb ^. address) (gb ^. gbRAM)
    address = gbCPU . cpuRegisterHL

dec :: Lens' CPU Word8 -> Gameboy -> Gameboy
dec reg gb =
  gb & gbCPU . reg -~ 1
    & gbCPU . cpuFlagH .~ (0x0F .&. gb ^. gbCPU . reg == 0x00)
    & gbCPU . cpuFlagZ .~ (gb ^. gbCPU . reg - 1 == 0x00)
    & gbCPU . cpuFlagN .~ True

decB :: Gameboy -> Gameboy
decB = dec cpuRegisterB

decC :: Gameboy -> Gameboy
decC = dec cpuRegisterC

decD :: Gameboy -> Gameboy
decD = dec cpuRegisterD

decE :: Gameboy -> Gameboy
decE = dec cpuRegisterE

decH :: Gameboy -> Gameboy
decH = dec cpuRegisterH

decL :: Gameboy -> Gameboy
decL = dec cpuRegisterL

decA :: Gameboy -> Gameboy
decA = dec cpuRegisterA

decHL_ :: Gameboy -> Gameboy
decHL_ gb =
  gb & writeToRam (gb ^. address) (val - 1)
    & gbCPU . cpuFlagH .~ (0x0F .&. val == 0x00)
    & gbCPU . cpuFlagZ .~ (val - 1 == 0x00)
    & gbCPU . cpuFlagN .~ True
  where
    val = ramLookup' (gb ^. address) (gb ^. gbRAM)
    address = gbCPU . cpuRegisterHL

addHL :: Lens' CPU Word16 -> Gameboy -> Gameboy
addHL reg gb =
  gb & gbCPU . cpuRegisterHL .~ op1 + op2
    & gbCPU . cpuFlagC .~ (op1 + op2 < op1)
    & gbCPU . cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
    & gbCPU . cpuFlagN .~ False
  where
    op1 = gb ^. gbCPU . cpuRegisterHL
    op2 = gb ^. gbCPU . reg

addHLBC :: Gameboy -> Gameboy
addHLBC = addHL cpuRegisterBC

addHLDE :: Gameboy -> Gameboy
addHLDE = addHL cpuRegisterDE

addHLHL :: Gameboy -> Gameboy
addHLHL = addHL cpuRegisterHL

addHLSP :: Gameboy -> Gameboy
addHLSP = addHL cpuSP

addSPr8 :: Gameboy -> Gameboy
addSPr8 gb = 
  (
    if r8 >= 0
      then 
        gb' & gbCPU . cpuSP .~ op1 + op2 
        & gbCPU . cpuFlagC .~ (op1 + op2 < op1)
        & gbCPU . cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
      else 
        gb' & gbCPU . cpuSP .~ op1 - op2 
        & gbCPU . cpuFlagC .~ (op1 - op2 > op1)
        & gbCPU . cpuFlagH .~ (0x0FFF .&. op1 - 0x0FFF .&. op2 < 0x0FFF) -- TODO: Is this correct??
  )
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
  where
    (gb', r8) = fromIntegral <$> pcLookup gb :: (Gameboy, Int8)
    op1 = gb ^. gbCPU . cpuSP
    op2 = fromIntegral (abs r8)

ldHLSPplusr8 :: Gameboy -> Gameboy
ldHLSPplusr8 gb =
  (
    if r8 >= 0
      then 
        gb' & gbCPU . cpuRegisterHL .~ op1 + op2 
        & gbCPU . cpuFlagC .~ (op1 + op2 < op1)
        & gbCPU . cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
      else 
        gb' & gbCPU . cpuRegisterHL .~ op1 - op2 
        & gbCPU . cpuFlagC .~ (op1 - op2 > op1)
        & gbCPU . cpuFlagH .~ (0x0FFF .&. op1 - 0x0FFF .&. op2 < 0x0FFF) -- TODO: Is this correct??
  )
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
  where
    (gb', r8) = fromIntegral <$> pcLookup gb :: (Gameboy, Int8)
    op1 = gb ^. gbCPU . cpuSP
    op2 = fromIntegral (abs r8)

addA :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
addA reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 + op2
    & gbCPU . cpuFlagZ .~ (op1 + op2 == 0x00)
    & gbCPU . cpuFlagH .~ (op1 + op2 < op1) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 + op2 < op1)
    & gbCPU . cpuFlagN .~ False
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg

addAB :: Gameboy -> Gameboy
addAB = addA cpuRegisterB

addAC :: Gameboy -> Gameboy
addAC = addA cpuRegisterC

addAD :: Gameboy -> Gameboy
addAD = addA cpuRegisterD

addAE :: Gameboy -> Gameboy
addAE = addA cpuRegisterE

addAH :: Gameboy -> Gameboy
addAH = addA cpuRegisterH

addAL :: Gameboy -> Gameboy
addAL = addA cpuRegisterL

addAHL :: Gameboy -> Gameboy
addAHL gb = addA (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

addAA :: Gameboy -> Gameboy
addAA = addA cpuRegisterA

addAd8 :: Gameboy -> Gameboy
addAd8 gb =
  gb' & gbCPU . cpuRegisterA .~ op1 + op2
    & gbCPU . cpuFlagZ .~ (op1 + op2 == 0x00)
    & gbCPU . cpuFlagH .~ (op1 + op2 < op1)
    & gbCPU . cpuFlagC .~ (op1 + op2 < op1)
    & gbCPU . cpuFlagN .~ False
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    (gb', op2) = pcLookup gb

adcA :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
adcA reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 + op2 + cy
    & gbCPU . cpuFlagZ .~ (op1 + op2 + cy == 0x00)
    & gbCPU . cpuFlagH .~ (op1 + op2 < op1 || (op1 + op2 + cy < op1)) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 + op2 < op1 || (op1 + op2 + cy < op1)) -- FIXME: There must be sth more elegant :/
    & gbCPU . cpuFlagN .~ False
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg
    cy = bool 0 1 $ gb ^. gbCPU . cpuFlagC

adcAB :: Gameboy -> Gameboy
adcAB = adcA cpuRegisterB

adcAC :: Gameboy -> Gameboy
adcAC = adcA cpuRegisterC

adcAD :: Gameboy -> Gameboy
adcAD = adcA cpuRegisterD

adcAE :: Gameboy -> Gameboy
adcAE = adcA cpuRegisterE

adcAH :: Gameboy -> Gameboy
adcAH = adcA cpuRegisterH

adcAL :: Gameboy -> Gameboy
adcAL = adcA cpuRegisterL

adcAHL :: Gameboy -> Gameboy
adcAHL gb = adcA (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

adcAA :: Gameboy -> Gameboy
adcAA = adcA cpuRegisterA

adcAd8 :: Gameboy -> Gameboy
adcAd8 gb = gb' & adcA (to (const d8))
  where
    (gb', d8) = pcLookup gb


sub :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
sub reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 - op2
    & gbCPU . cpuFlagZ .~ (op1 - op2 == 0x00)
    & gbCPU . cpuFlagH .~ (op1 - op2 > op1) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 - op2 > op1)
    & gbCPU . cpuFlagN .~ True
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg

subB :: Gameboy -> Gameboy
subB = sub cpuRegisterB

subC :: Gameboy -> Gameboy
subC = sub cpuRegisterC

subD :: Gameboy -> Gameboy
subD = sub cpuRegisterD

subE :: Gameboy -> Gameboy
subE = sub cpuRegisterE

subH :: Gameboy -> Gameboy
subH = sub cpuRegisterH

subL :: Gameboy -> Gameboy
subL = sub cpuRegisterL

subHL :: Gameboy -> Gameboy
subHL gb = sub (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

subA :: Gameboy -> Gameboy
subA = sub cpuRegisterA

subd8 :: Gameboy -> Gameboy
subd8 gb = gb' & sub (to (const d8))
  where
    (gb', d8) = pcLookup gb

sbc :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
sbc reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 - op2 - cy
    & gbCPU . cpuFlagZ .~ (op1 - op2 - cy == 0x00)
    & gbCPU . cpuFlagH .~ (op1 - op2 > op1 || op1 - op2 - cy > op1) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 - op2 > op1 || op1 - op2 - cy > op1)
    & gbCPU . cpuFlagN .~ True
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg
    cy = bool 0 1 $ gb ^. gbCPU . cpuFlagC

sbcB :: Gameboy -> Gameboy
sbcB = sbc cpuRegisterB

sbcC :: Gameboy -> Gameboy
sbcC = sbc cpuRegisterC

sbcD :: Gameboy -> Gameboy
sbcD = sbc cpuRegisterD

sbcE :: Gameboy -> Gameboy
sbcE = sbc cpuRegisterE

sbcH :: Gameboy -> Gameboy
sbcH = sbc cpuRegisterH

sbcL :: Gameboy -> Gameboy
sbcL = sbc cpuRegisterL

sbcHL :: Gameboy -> Gameboy
sbcHL gb = sbc (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

sbcA :: Gameboy -> Gameboy
sbcA = sbc cpuRegisterA

sbcAd8 :: Gameboy -> Gameboy
sbcAd8 gb = gb' & sbc (to (const d8))
  where
    (gb', d8) = pcLookup gb

aAnd :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
aAnd reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 .&. op2
    & gbCPU . cpuFlagZ .~ (op1 .&. op2 == 0x00)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ True
    & gbCPU . cpuFlagC .~ False
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg


andB :: Gameboy -> Gameboy
andB = aAnd cpuRegisterB

andC :: Gameboy -> Gameboy
andC = aAnd cpuRegisterC

andD :: Gameboy -> Gameboy
andD = aAnd cpuRegisterD

andE :: Gameboy -> Gameboy
andE = aAnd cpuRegisterE

andH :: Gameboy -> Gameboy
andH = aAnd cpuRegisterH

andL :: Gameboy -> Gameboy
andL = aAnd cpuRegisterL

andHL :: Gameboy -> Gameboy
andHL gb = aAnd (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

andA :: Gameboy -> Gameboy
andA = aAnd cpuRegisterA

andd8 :: Gameboy -> Gameboy
andd8 gb = gb' & aAnd (to (const d8))
  where
    (gb', d8) = pcLookup gb

aXor :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
aXor reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 `xor` op2
    & gbCPU . cpuFlagZ .~ (op1 `xor` op2 == 0x00)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
    & gbCPU . cpuFlagC .~ False
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg

xorB :: Gameboy -> Gameboy
xorB = aXor cpuRegisterB

xorC :: Gameboy -> Gameboy
xorC = aXor cpuRegisterC

xorD :: Gameboy -> Gameboy
xorD = aXor cpuRegisterD

xorE :: Gameboy -> Gameboy
xorE = aXor cpuRegisterE

xorH :: Gameboy -> Gameboy
xorH = aXor cpuRegisterH

xorL :: Gameboy -> Gameboy
xorL = aXor cpuRegisterL

xorHL :: Gameboy -> Gameboy
xorHL gb = aXor (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

xorA :: Gameboy -> Gameboy
xorA = aXor cpuRegisterA

xord8 :: Gameboy -> Gameboy
xord8 gb = gb' & aXor (to (const d8))
  where
    (gb', d8) = pcLookup gb

aor :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
aor reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 .|. op2
    & gbCPU . cpuFlagZ .~ (op1 .|. op2 == 0x00)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
    & gbCPU . cpuFlagC .~ False
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg

orB :: Gameboy -> Gameboy
orB = aor cpuRegisterB

orC :: Gameboy -> Gameboy
orC = aor cpuRegisterC

orD :: Gameboy -> Gameboy
orD = aor cpuRegisterD

orE :: Gameboy -> Gameboy
orE = aor cpuRegisterE

orH :: Gameboy -> Gameboy
orH = aor cpuRegisterH

orL :: Gameboy -> Gameboy
orL = aor cpuRegisterL

orHL :: Gameboy -> Gameboy
orHL gb = aor (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

orA :: Gameboy -> Gameboy
orA = aor cpuRegisterA

ord8 :: Gameboy -> Gameboy
ord8 gb = gb' & aor (to (const d8))
  where
    (gb', d8) = pcLookup gb

-- NOTE: This is just sub without writing into A. Consider seperation of flag effects, operations, and actual writing of registers?
acp :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
acp reg gb =
  gb & gbCPU . cpuFlagZ .~ (op1 == op2)
    & gbCPU . cpuFlagN .~ True
    & gbCPU . cpuFlagH .~ (op1 - op2 > op1)
    & gbCPU . cpuFlagC .~ (op1 - op2 > op1)
  where
    op1 = gb ^. gbCPU . cpuRegisterA
    op2 = gb ^. gbCPU . reg

cpB :: Gameboy -> Gameboy
cpB = acp cpuRegisterB

cpC :: Gameboy -> Gameboy
cpC = acp cpuRegisterC

cpD :: Gameboy -> Gameboy
cpD = acp cpuRegisterD

cpE :: Gameboy -> Gameboy
cpE = acp cpuRegisterE

cpH :: Gameboy -> Gameboy
cpH = acp cpuRegisterH

cpL :: Gameboy -> Gameboy
cpL = acp cpuRegisterL

cpHL :: Gameboy -> Gameboy
cpHL gb = acp (cpuRegisterHL . to (`ramLookup'` (gb ^. gbRAM))) gb

cpA :: Gameboy -> Gameboy
cpA = acp cpuRegisterA

cpd8 :: Gameboy -> Gameboy
cpd8 gb = gb' & acp (to (const d8))
  where
    (gb', d8) = pcLookup gb

jrr8 :: Gameboy -> Gameboy
jrr8 gb =
  gb' & ( if r8 < 0
          then gbCPU . cpuPC -~ fromIntegral (abs r8)
          else gbCPU . cpuPC +~ fromIntegral r8
      )
  where
    (gb', r8) = fromIntegral <$> pcLookup gb :: (Gameboy, Int8)

-- TODO: Consider whether implementation in terms of jrr8 is a good idea or not...
jrZr8 :: Gameboy -> Gameboy
jrZr8 gb =
  if gb ^. gbCPU . cpuFlagZ
    then jrr8 gb
    else fst . pcLookup $ gb -- read it and throw it away...

jrNZr8 :: Gameboy -> Gameboy
jrNZr8 gb =
  if not (gb ^. gbCPU . cpuFlagZ)
    then jrr8 gb
    else fst . pcLookup $ gb

jrCr8 :: Gameboy -> Gameboy
jrCr8 gb =
  if gb ^. gbCPU . cpuFlagC
    then jrr8 gb
    else fst . pcLookup $ gb

jrNCr8 :: Gameboy -> Gameboy
jrNCr8 gb =
  if not (gb ^. gbCPU . cpuFlagC)
    then jrr8 gb
    else fst . pcLookup $ gb

jpHL :: Gameboy -> Gameboy
jpHL gb = 
  gb & gbCPU . cpuPC .~ (gb ^. gbCPU . cpuRegisterHL)

jpa16 :: Gameboy -> Gameboy
jpa16 gb = gb'' & gbCPU . cpuPC .~ mkWord16 msb lsb
  where
    (gb', lsb) = pcLookup gb
    (gb'', msb) = pcLookup gb'

jpZa16 :: Gameboy -> Gameboy
jpZa16 gb =
  if gb ^. gbCPU . cpuFlagZ
    then jpa16 gb
    else fst . pcLookup . fst . pcLookup $ gb

jpNZa16 :: Gameboy -> Gameboy
jpNZa16 gb =
  if not (gb ^. gbCPU . cpuFlagZ)
    then jpa16 gb
    else fst . pcLookup . fst . pcLookup $ gb

jpCa16 :: Gameboy -> Gameboy
jpCa16 gb =
  if gb ^. gbCPU . cpuFlagC
    then jpa16 gb
    else fst . pcLookup . fst . pcLookup $ gb

jpNCa16 :: Gameboy -> Gameboy
jpNCa16 gb =
  if not (gb ^. gbCPU . cpuFlagC)
    then jpa16 gb
    else fst . pcLookup . fst . pcLookup $ gb

cpl :: Gameboy -> Gameboy
cpl gb =
  gb & gbCPU . cpuRegisterA %~ complement
    & gbCPU . cpuFlagN .~ True
    & gbCPU . cpuFlagH .~ True

scf :: Gameboy -> Gameboy
scf gb =
  gb & gbCPU . cpuFlagC .~ True
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False

ccf :: Gameboy -> Gameboy
ccf gb =
  gb & gbCPU . cpuFlagC %~ not
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False

ret :: Gameboy -> Gameboy
ret gb = 
  gb & gbCPU . cpuPC .~ mkWord16 msb lsb
    & gbCPU . cpuSP +~ 2
  where
    lsb = ramLookup' (gb ^. gbCPU . cpuSP) (gb ^. gbRAM)
    msb = ramLookup' (gb ^. gbCPU . cpuSP + 1) (gb ^. gbRAM)

retZ :: Gameboy -> Gameboy
retZ gb =
  if gb ^. gbCPU . cpuFlagZ
    then ret gb
    else gb

retNZ :: Gameboy -> Gameboy
retNZ gb =
  if not (gb ^. gbCPU . cpuFlagZ)
    then ret gb
    else gb

retC :: Gameboy -> Gameboy
retC gb =
  if gb ^. gbCPU . cpuFlagC
    then ret gb
    else gb

retNC :: Gameboy -> Gameboy
retNC gb =
  if not (gb ^. gbCPU . cpuFlagC)
    then ret gb
    else gb

pop :: Lens' CPU Word16 -> Gameboy -> Gameboy
pop reg gb =
  gb & gbCPU . reg .~ mkWord16 msb lsb
    & gbCPU . cpuSP +~ 2
  where
    lsb = ramLookup' (gb ^. gbCPU . cpuSP) (gb ^. gbRAM)
    msb = ramLookup' (gb ^. gbCPU . cpuSP + 1) (gb ^. gbRAM)

popBC :: Gameboy -> Gameboy
popBC = pop cpuRegisterBC

popDE :: Gameboy -> Gameboy
popDE = pop cpuRegisterDE

popHL :: Gameboy -> Gameboy
popHL = pop cpuRegisterHL

popAF :: Gameboy -> Gameboy
popAF = pop cpuRegisterAF

push :: Lens' CPU Word16 -> Gameboy -> Gameboy
push reg gb =
  gb & writeToRam (gb ^. gbCPU . cpuSP - 1) msb
    & writeToRam (gb ^. gbCPU . cpuSP - 2) lsb
    & gbCPU . cpuSP -~ 2
  where
    (msb, lsb) = splitWord16 $ gb ^. gbCPU . reg

pushBC :: Gameboy -> Gameboy
pushBC = push cpuRegisterBC

pushDE :: Gameboy -> Gameboy
pushDE = push cpuRegisterDE

pushHL :: Gameboy -> Gameboy
pushHL = push cpuRegisterHL

pushAF :: Gameboy -> Gameboy
pushAF = push cpuRegisterAF

calla16 :: Gameboy -> Gameboy
calla16 gb = 
    gb'' & writeToRam (gb ^. gbCPU . cpuSP - 1) pcmsb
      & writeToRam (gb ^. gbCPU . cpuSP - 2) pclsb
      & gbCPU . cpuSP -~ 2
      & gbCPU . cpuPC .~ mkWord16 targetmsb targetlsb
  where
    (pcmsb, pclsb) = splitWord16 $ gb ^. gbCPU . cpuPC
    (gb', targetlsb) = pcLookup gb
    (gb'', targetmsb) = pcLookup gb'

callZa16 :: Gameboy -> Gameboy
callZa16 gb =
  if gb ^. gbCPU . cpuFlagZ
    then calla16 gb
    else fst . pcLookup . fst . pcLookup $ gb

callNZa16 :: Gameboy -> Gameboy
callNZa16 gb =
  if not (gb ^. gbCPU . cpuFlagZ)
    then calla16 gb
    else fst . pcLookup . fst . pcLookup $ gb

callCa16 :: Gameboy -> Gameboy
callCa16 gb =
  if gb ^. gbCPU . cpuFlagC
    then calla16 gb
    else fst . pcLookup . fst . pcLookup $ gb

callNCa16 :: Gameboy -> Gameboy
callNCa16 gb =
  if not (gb ^. gbCPU . cpuFlagC)
    then calla16 gb
    else fst . pcLookup . fst . pcLookup $ gb

rst :: Word8 -> Gameboy -> Gameboy
rst lsb gb =
  gb & writeToRam (gb ^. gbCPU . cpuSP - 1) pcmsb
    & writeToRam (gb ^. gbCPU . cpuSP - 2) pclsb
    & gbCPU . cpuSP -~ 2
    & gbCPU . cpuPC .~ mkWord16 0x00 lsb
  where
    (pcmsb, pclsb) = splitWord16 $ gb ^. gbCPU . cpuPC

rst00 :: Gameboy -> Gameboy
rst00 = rst 0x00

rst08 :: Gameboy -> Gameboy
rst08 = rst 0x08

rst10 :: Gameboy -> Gameboy
rst10 = rst 0x10

rst18 :: Gameboy -> Gameboy
rst18 = rst 0x18

rst20 :: Gameboy -> Gameboy
rst20 = rst 0x20

rst28 :: Gameboy -> Gameboy
rst28 = rst 0x28

rst30 :: Gameboy -> Gameboy
rst30 = rst 0x30

rst38 :: Gameboy -> Gameboy
rst38 = rst 0x38

ei :: Gameboy -> Gameboy
ei gb = gb & gbIME .~ True

di :: Gameboy -> Gameboy
di gb = gb & gbIME .~ False

reti :: Gameboy -> Gameboy
reti = ret . ei

-- Note: The pandocs classify ldhCA and ldhAC as 2-Byte-instructions - however it is not at all clear where the read of the 2nd byte should occur
--       This being two byte is either a bug in the cpu or a mistake in the pandocs - currently going for the latter
ldhCA :: Gameboy -> Gameboy
ldhCA gb = gb & writeToRam (mkWord16 0xFF lsb) (gb ^. gbCPU . cpuRegisterA)
  where
    lsb = gb ^. gbCPU . cpuRegisterC

ldhAC :: Gameboy -> Gameboy
ldhAC gb = gb & gbCPU . cpuRegisterA .~ newVal
  where
    newVal = ramLookup' (mkWord16 0xFF lsb) (gb ^. gbRAM)
    lsb = gb ^. gbCPU . cpuRegisterC

ldha8A :: Gameboy -> Gameboy
ldha8A gb = gb' & writeToRam (mkWord16 0xFF lsb) (gb ^. gbCPU . cpuRegisterA)
  where
    (gb', lsb) = pcLookup gb

ldhAa8 :: Gameboy -> Gameboy
ldhAa8 gb = gb' & gbCPU . cpuRegisterA .~ newVal
  where
    newVal = ramLookup' (mkWord16 0xFF lsb) (gb ^. gbRAM)
    (gb', lsb) = pcLookup gb