{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens
import Data.Bits (bit, complement, rotateL, rotateR, setBit, shiftL, shiftR, (.&.), (.|.))
import Data.Bool (bool)
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
import Data.Word

data Gameboy = Gameboy
  { _gbCPU :: CPU,
    _gbRAM :: RAM
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

bit7 :: Word8
bit7 = 0x01 `shiftL` 7

bit6 :: Word8
bit6 = 0x01 `shiftL` 6

bit5 :: Word8
bit5 = 0x01 `shiftL` 5

bit4 :: Word8
bit4 = 0x01 `shiftL` 4

bit3 :: Word8
bit3 = 0x01 `shiftL` 3

bit2 :: Word8
bit2 = 0x01 `shiftL` 2

bit1 :: Word8
bit1 = 0x01 `shiftL` 1

bit0 :: Word8
bit0 = 0x01 `shiftL` 0

bitwiseValue :: Word8 -> Lens' Word8 Bool
bitwiseValue mask = lens getter setter
  where
    getter w = (mask .&. w) > 0
    setter w = bool (w .&. complement mask) (w .|. mask)

cpuFlagZ :: Lens' CPU Bool
cpuFlagZ = cpuRegisterF . bitwiseValue bit7

cpuFlagN :: Lens' CPU Bool
cpuFlagN = cpuRegisterF . bitwiseValue bit6

cpuFlagH :: Lens' CPU Bool
cpuFlagH = cpuRegisterF . bitwiseValue bit5

cpuFlagC :: Lens' CPU Bool
cpuFlagC = cpuRegisterF . bitwiseValue bit4

cpuRegisterHL :: Lens' CPU Word16
cpuRegisterHL = lens getter setter
  where
    getter cpu = mkWord16 (_cpuRegisterH cpu) (_cpuRegisterL cpu)
    setter cpu w = cpu {_cpuRegisterH = fst . splitWord16 $ w, _cpuRegisterL = snd . splitWord16 $ w}

cpuRegisterBC :: Lens' CPU Word16
cpuRegisterBC = lens getter setter
  where
    getter cpu = mkWord16 (_cpuRegisterB cpu) (_cpuRegisterC cpu)
    setter cpu w = cpu {_cpuRegisterB = fst . splitWord16 $ w, _cpuRegisterC = snd . splitWord16 $ w}

cpuRegisterDE :: Lens' CPU Word16
cpuRegisterDE = lens getter setter
  where
    getter cpu = mkWord16 (_cpuRegisterD cpu) (_cpuRegisterE cpu)
    setter cpu w = cpu {_cpuRegisterD = fst . splitWord16 $ w, _cpuRegisterE = snd . splitWord16 $ w}

initGameboy :: Gameboy
initGameboy =
  Gameboy
    { _gbCPU = initCpu,
      _gbRAM = mempty
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

ramLookup :: Word16 -> RAM -> Word8
ramLookup l ram =
  case M.lookup l ram of
    (Just v) -> v
    Nothing -> 0xFF -- TODO: Check if good default

writeToRam :: Word16 -> Word8 -> Gameboy -> Gameboy
writeToRam k v gb = gb & gbRAM . at k ?~ v

runInstruction :: Gameboy -> Gameboy
runInstruction gb =
  -- fetch
  let ni = ramLookup (gb ^. gbCPU . cpuPC) (gb ^. gbRAM)
   in -- decode
      -- execute: return GB with executed instruction and updated ProgramCounter
      execInstruction ni gb

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
    0x27 -> undefined -- TODO: DAA
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
    _ -> undefined

nop :: Gameboy -> Gameboy
nop = id

-- TODO: Halt until interrupt occurs!
halt :: Gameboy -> Gameboy
halt gb = undefined & gbCPU . cpuPC +~ 1

-- TODO: Low power standby - whatever that is
-- NOTE: From the specs this should encode as 0x1000, i.e. 2 bytes - since this is unnecessary it is 'sometimes' encoded simply as 0x10
--       - might need to check whether PC should be incremented by 1 or 2....
stop :: Gameboy -> Gameboy
stop gb = undefined & gbCPU . cpuPC +~ 2

ldBA :: Gameboy -> Gameboy
ldBA gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterA) & gbCPU . cpuPC +~ 1

-- Not sure if it actually needs to be implemented in terms other than nop, but here we go
ldBB :: Gameboy -> Gameboy
ldBB gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterB) & gbCPU . cpuPC +~ 1

ldBC :: Gameboy -> Gameboy
ldBC gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterC) & gbCPU . cpuPC +~ 1

ldBD :: Gameboy -> Gameboy
ldBD gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterD) & gbCPU . cpuPC +~ 1

ldBE :: Gameboy -> Gameboy
ldBE gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterE) & gbCPU . cpuPC +~ 1

ldBH :: Gameboy -> Gameboy
ldBH gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterH) & gbCPU . cpuPC +~ 1

ldBL :: Gameboy -> Gameboy
ldBL gb = gb & gbCPU . cpuRegisterB .~ (gb ^. gbCPU . cpuRegisterL) & gbCPU . cpuPC +~ 1

ldBHL :: Gameboy -> Gameboy
ldBHL gb = gb & gbCPU . cpuRegisterB .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldCA :: Gameboy -> Gameboy
ldCA gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterA) & gbCPU . cpuPC +~ 1

ldCB :: Gameboy -> Gameboy
ldCB gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterB) & gbCPU . cpuPC +~ 1

ldCC :: Gameboy -> Gameboy
ldCC gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterC) & gbCPU . cpuPC +~ 1

ldCD :: Gameboy -> Gameboy
ldCD gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterD) & gbCPU . cpuPC +~ 1

ldCE :: Gameboy -> Gameboy
ldCE gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterE) & gbCPU . cpuPC +~ 1

ldCH :: Gameboy -> Gameboy
ldCH gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterH) & gbCPU . cpuPC +~ 1

ldCL :: Gameboy -> Gameboy
ldCL gb = gb & gbCPU . cpuRegisterC .~ (gb ^. gbCPU . cpuRegisterL) & gbCPU . cpuPC +~ 1

ldCHL :: Gameboy -> Gameboy
ldCHL gb = gb & gbCPU . cpuRegisterC .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldDA :: Gameboy -> Gameboy
ldDA gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterA) & gbCPU . cpuPC +~ 1

ldDB :: Gameboy -> Gameboy
ldDB gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterB) & gbCPU . cpuPC +~ 1

ldDC :: Gameboy -> Gameboy
ldDC gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterC) & gbCPU . cpuPC +~ 1

ldDD :: Gameboy -> Gameboy
ldDD gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterD) & gbCPU . cpuPC +~ 1

ldDE :: Gameboy -> Gameboy
ldDE gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterE) & gbCPU . cpuPC +~ 1

ldDH :: Gameboy -> Gameboy
ldDH gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterH) & gbCPU . cpuPC +~ 1

ldDL :: Gameboy -> Gameboy
ldDL gb = gb & gbCPU . cpuRegisterD .~ (gb ^. gbCPU . cpuRegisterL) & gbCPU . cpuPC +~ 1

ldDHL :: Gameboy -> Gameboy
ldDHL gb = gb & gbCPU . cpuRegisterD .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldEA :: Gameboy -> Gameboy
ldEA gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterA) & gbCPU . cpuPC +~ 1

ldEB :: Gameboy -> Gameboy
ldEB gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterB) & gbCPU . cpuPC +~ 1

ldEC :: Gameboy -> Gameboy
ldEC gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterC) & gbCPU . cpuPC +~ 1

ldED :: Gameboy -> Gameboy
ldED gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterD) & gbCPU . cpuPC +~ 1

ldEE :: Gameboy -> Gameboy
ldEE gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterE) & gbCPU . cpuPC +~ 1

ldEH :: Gameboy -> Gameboy
ldEH gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterH) & gbCPU . cpuPC +~ 1

ldEL :: Gameboy -> Gameboy
ldEL gb = gb & gbCPU . cpuRegisterE .~ (gb ^. gbCPU . cpuRegisterL) & gbCPU . cpuPC +~ 1

ldEHL :: Gameboy -> Gameboy
ldEHL gb = gb & gbCPU . cpuRegisterE .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldHA :: Gameboy -> Gameboy
ldHA gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterA) & gbCPU . cpuPC +~ 1

ldHB :: Gameboy -> Gameboy
ldHB gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterB) & gbCPU . cpuPC +~ 1

ldHC :: Gameboy -> Gameboy
ldHC gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterC) & gbCPU . cpuPC +~ 1

ldHD :: Gameboy -> Gameboy
ldHD gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterD) & gbCPU . cpuPC +~ 1

ldHE :: Gameboy -> Gameboy
ldHE gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterE) & gbCPU . cpuPC +~ 1

ldHH :: Gameboy -> Gameboy
ldHH gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterH) & gbCPU . cpuPC +~ 1

ldHL :: Gameboy -> Gameboy
ldHL gb = gb & gbCPU . cpuRegisterH .~ (gb ^. gbCPU . cpuRegisterL) & gbCPU . cpuPC +~ 1

ldHHL :: Gameboy -> Gameboy
ldHHL gb = gb & gbCPU . cpuRegisterH .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldLA :: Gameboy -> Gameboy
ldLA gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterA) & gbCPU . cpuPC +~ 1

ldLB :: Gameboy -> Gameboy
ldLB gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterB) & gbCPU . cpuPC +~ 1

ldLC :: Gameboy -> Gameboy
ldLC gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterC) & gbCPU . cpuPC +~ 1

ldLD :: Gameboy -> Gameboy
ldLD gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterD) & gbCPU . cpuPC +~ 1

ldLE :: Gameboy -> Gameboy
ldLE gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterE) & gbCPU . cpuPC +~ 1

ldLH :: Gameboy -> Gameboy
ldLH gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterH) & gbCPU . cpuPC +~ 1

ldLL :: Gameboy -> Gameboy
ldLL gb = gb & gbCPU . cpuRegisterL .~ (gb ^. gbCPU . cpuRegisterL) & gbCPU . cpuPC +~ 1

ldLHL :: Gameboy -> Gameboy
ldLHL gb = gb & gbCPU . cpuRegisterL .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldHLA :: Gameboy -> Gameboy
ldHLA gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterA) gb & gbCPU . cpuPC +~ 1

ldHLB :: Gameboy -> Gameboy
ldHLB gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterB) gb & gbCPU . cpuPC +~ 1

ldHLC :: Gameboy -> Gameboy
ldHLC gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterC) gb & gbCPU . cpuPC +~ 1

ldHLD :: Gameboy -> Gameboy
ldHLD gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterD) gb & gbCPU . cpuPC +~ 1

ldHLE :: Gameboy -> Gameboy
ldHLE gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterE) gb & gbCPU . cpuPC +~ 1

ldHLH :: Gameboy -> Gameboy
ldHLH gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterH) gb & gbCPU . cpuPC +~ 1

ldHLL :: Gameboy -> Gameboy
ldHLL gb = writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterL) gb & gbCPU . cpuPC +~ 1

ldAA :: Gameboy -> Gameboy
ldAA gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterA) & gbCPU . cpuPC +~ 1

ldAB :: Gameboy -> Gameboy
ldAB gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterB) & gbCPU . cpuPC +~ 1

ldAC :: Gameboy -> Gameboy
ldAC gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterC) & gbCPU . cpuPC +~ 1

ldAD :: Gameboy -> Gameboy
ldAD gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterD) & gbCPU . cpuPC +~ 1

ldAE :: Gameboy -> Gameboy
ldAE gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterE) & gbCPU . cpuPC +~ 1

ldAH :: Gameboy -> Gameboy
ldAH gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterH) & gbCPU . cpuPC +~ 1

ldAL :: Gameboy -> Gameboy
ldAL gb = gb & gbCPU . cpuRegisterA .~ (gb ^. gbCPU . cpuRegisterL) & gbCPU . cpuPC +~ 1

ldAHL :: Gameboy -> Gameboy
ldAHL gb = gb & gbCPU . cpuRegisterA .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldABC :: Gameboy -> Gameboy
ldABC gb = gb & gbCPU . cpuRegisterA .~ ramLookup (gb ^. gbCPU . cpuRegisterBC) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldADE :: Gameboy -> Gameboy
ldADE gb = gb & gbCPU . cpuRegisterA .~ ramLookup (gb ^. gbCPU . cpuRegisterDE) (gb ^. gbRAM) & gbCPU . cpuPC +~ 1

ldBCA :: Gameboy -> Gameboy
ldBCA gb = writeToRam (gb ^. gbCPU . cpuRegisterBC) (gb ^. gbCPU . cpuRegisterA) gb & gbCPU . cpuPC +~ 1

ldDEA :: Gameboy -> Gameboy
ldDEA gb = writeToRam (gb ^. gbCPU . cpuRegisterDE) (gb ^. gbCPU . cpuRegisterA) gb & gbCPU . cpuPC +~ 1

ldHLplusA :: Gameboy -> Gameboy
ldHLplusA gb =
  writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterA) gb
    & gbCPU . cpuRegisterHL +~ 1
    & gbCPU . cpuPC +~ 1

ldHLminusA :: Gameboy -> Gameboy
ldHLminusA gb =
  writeToRam (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbCPU . cpuRegisterA) gb
    & gbCPU . cpuRegisterHL -~ 1
    & gbCPU . cpuPC +~ 1

ldAHLplus :: Gameboy -> Gameboy
ldAHLplus gb =
  gb & gbCPU . cpuRegisterA .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)
    & gbCPU . cpuRegisterHL +~ 1
    & gbCPU . cpuPC +~ 1

ldAHLminus :: Gameboy -> Gameboy
ldAHLminus gb =
  gb & gbCPU . cpuRegisterA .~ ramLookup (gb ^. gbCPU . cpuRegisterHL) (gb ^. gbRAM)
    & gbCPU . cpuRegisterHL -~ 1
    & gbCPU . cpuPC +~ 1

ldd8 :: Lens' CPU Word8 -> Gameboy -> Gameboy
ldd8 reg gb =
  gb & gbCPU . reg .~ ramLookup (gb ^. gbCPU . cpuPC + 1) (gb ^. gbRAM)
    & gbCPU . cpuPC +~ 2

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
ldHLd8 gb =
  gb & writeToRam (gb ^. gbCPU . cpuRegisterHL) (ramLookup (gb ^. gbCPU . cpuPC + 1) (gb ^. gbRAM))
    & gbCPU . cpuPC +~ 2

ldd16 :: Lens' CPU Word16 -> Gameboy -> Gameboy
ldd16 reg gb =
  gb & gbCPU . reg .~ mkWord16 msb lsb
    & gbCPU . cpuPC +~ 3
  where
    lsb = ramLookup (gb ^. gbCPU . cpuPC + 1) (gb ^. gbRAM)
    msb = ramLookup (gb ^. gbCPU . cpuPC + 2) (gb ^. gbRAM)

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
  gb & writeToRam (mkWord16 msb lsb) splsb
    & writeToRam (mkWord16 msb lsb + 1) spmsb
    & gbCPU . cpuPC +~ 3
  where
    (spmsb, splsb) = splitWord16 (gb ^. gbCPU . cpuSP)
    lsb = ramLookup (gb ^. gbCPU . cpuPC + 1) (gb ^. gbRAM)
    msb = ramLookup (gb ^. gbCPU . cpuPC + 2) (gb ^. gbRAM)

incBC :: Gameboy -> Gameboy
incBC gb = gb & gbCPU . cpuRegisterBC +~ 1 & gbCPU . cpuPC +~ 1

incDE :: Gameboy -> Gameboy
incDE gb = gb & gbCPU . cpuRegisterDE +~ 1 & gbCPU . cpuPC +~ 1

incHL :: Gameboy -> Gameboy
incHL gb = gb & gbCPU . cpuRegisterHL +~ 1 & gbCPU . cpuPC +~ 1

incSP :: Gameboy -> Gameboy
incSP gb = gb & gbCPU . cpuSP +~ 1 & gbCPU . cpuPC +~ 1

decBC :: Gameboy -> Gameboy
decBC gb = gb & gbCPU . cpuRegisterBC -~ 1 & gbCPU . cpuPC +~ 1

decDE :: Gameboy -> Gameboy
decDE gb = gb & gbCPU . cpuRegisterDE -~ 1 & gbCPU . cpuPC +~ 1

decHL :: Gameboy -> Gameboy
decHL gb = gb & gbCPU . cpuRegisterHL -~ 1 & gbCPU . cpuPC +~ 1

decSP :: Gameboy -> Gameboy
decSP gb = gb & gbCPU . cpuSP -~ 1 & gbCPU . cpuPC +~ 1

rlA :: Gameboy -> Gameboy
rlA gb =
  gb & gbCPU . cpuRegisterA . bitwiseValue bit7 .~ new7th -- swap 7th bit with carry so we can ignore carry when rotating
    & gbCPU . cpuFlagC .~ old7th
    & gbCPU . cpuRegisterA %~ (`rotateL` 1)
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
    & gbCPU . cpuPC +~ 1
  where
    old7th = gb ^. gbCPU . cpuRegisterA . bitwiseValue bit7
    new7th = gb ^. gbCPU . cpuFlagC

rrA :: Gameboy -> Gameboy
rrA gb =
  gb & gbCPU . cpuRegisterA . bitwiseValue bit0 .~ new0th -- swap 0th bit with carry so we can ignore carry when rotating
    & gbCPU . cpuFlagC .~ old0th
    & gbCPU . cpuRegisterA %~ (`rotateR` 1)
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
    & gbCPU . cpuPC +~ 1
  where
    old0th = gb ^. gbCPU . cpuRegisterA . bitwiseValue bit0
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
    & gbCPU . cpuPC +~ 1
  where
    old7th = gb ^. gbCPU . cpuRegisterA . bitwiseValue bit7

rrcA :: Gameboy -> Gameboy
rrcA gb =
  gb & gbCPU . cpuFlagC .~ old0th
    & gbCPU . cpuRegisterA %~ (`rotateR` 1)
    & gbCPU . cpuFlagZ .~ False
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
    & gbCPU . cpuPC +~ 1
  where
    old0th = gb ^. gbCPU . cpuRegisterA . bitwiseValue bit0

inc :: Lens' CPU Word8 -> Gameboy -> Gameboy
inc reg gb =
  gb & gbCPU . reg +~ 1
    & gbCPU . cpuFlagH .~ (0x0F .&. gb ^. gbCPU . reg + 1 > 0x0F)
    & gbCPU . cpuFlagZ .~ (gb ^. gbCPU . reg + 1 == 0x00)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuPC +~ 1

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
    & gbCPU . cpuPC +~ 1
  where
    val = ramLookup (gb ^. address) (gb ^. gbRAM)
    address = gbCPU . cpuRegisterHL

dec :: Lens' CPU Word8 -> Gameboy -> Gameboy
dec reg gb =
  gb & gbCPU . reg -~ 1
    & gbCPU . cpuFlagH .~ (0x0F .&. gb ^. gbCPU . reg == 0x00)
    & gbCPU . cpuFlagZ .~ (gb ^. gbCPU . reg - 1 == 0x00)
    & gbCPU . cpuFlagN .~ True
    & gbCPU . cpuPC +~ 1

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
    & gbCPU . cpuPC +~ 1
  where
    val = ramLookup (gb ^. address) (gb ^. gbRAM)
    address = gbCPU . cpuRegisterHL

addHL :: Lens' CPU Word16 -> Gameboy -> Gameboy
addHL reg gb =
  gb & gbCPU . cpuRegisterHL .~ op1 + op2
    & gbCPU . cpuFlagC .~ (op1 + op2 < op1)
    & gbCPU . cpuFlagH .~ (0x0FFF .&. op1 + 0x0FFF .&. op2 > 0x0FFF)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuPC +~ 1
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

addA :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
addA reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 + op2
    & gbCPU . cpuFlagZ .~ (op1 + op2 == 0x00)
    & gbCPU . cpuFlagH .~ (op1 + op2 < op1) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 + op2 < op1)
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuPC +~ 1
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
addAHL gb = addA (cpuRegisterHL . to (`ramLookup` (gb ^. gbRAM))) gb

addAA :: Gameboy -> Gameboy
addAA = addA cpuRegisterA

adcA :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
adcA reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 + op2 + cy
    & gbCPU . cpuFlagZ .~ (op1 + op2 + cy == 0x00)
    & gbCPU . cpuFlagH .~ (op1 + op2 < op1 || (op1 + op2 + cy < op1)) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 + op2 < op1 || (op1 + op2 + cy < op1)) -- FIXME: There must be sth more elegant :/
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuPC +~ 1
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
adcAHL gb = adcA (cpuRegisterHL . to (`ramLookup` (gb ^. gbRAM))) gb

adcAA :: Gameboy -> Gameboy
adcAA = adcA cpuRegisterA

sub :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
sub reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 - op2
    & gbCPU . cpuFlagZ .~ (op1 - op2 == 0x00)
    & gbCPU . cpuFlagH .~ (op1 - op2 > op1) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 - op2 > op1)
    & gbCPU . cpuFlagN .~ True
    & gbCPU . cpuPC +~ 1
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
subHL gb = sub (cpuRegisterHL . to (`ramLookup` (gb ^. gbRAM))) gb

subA :: Gameboy -> Gameboy
subA = sub cpuRegisterA

sbc :: Getting Word8 CPU Word8 -> Gameboy -> Gameboy
sbc reg gb =
  gb & gbCPU . cpuRegisterA .~ op1 - op2 - cy
    & gbCPU . cpuFlagZ .~ (op1 - op2 - cy == 0x00)
    & gbCPU . cpuFlagH .~ (op1 - op2 > op1 || op1 - op2 - cy > op1) -- I suppose H in this context is just C...?
    & gbCPU . cpuFlagC .~ (op1 - op2 > op1 || op1 - op2 - cy > op1)
    & gbCPU . cpuFlagN .~ True
    & gbCPU . cpuPC +~ 1
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
sbcHL gb = sbc (cpuRegisterHL . to (`ramLookup` (gb ^. gbRAM))) gb

sbcA :: Gameboy -> Gameboy
sbcA = sbc cpuRegisterA

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
andHL gb = aAnd (cpuRegisterHL . to (`ramLookup` (gb ^. gbRAM))) gb

andA :: Gameboy -> Gameboy
andA = aAnd cpuRegisterA

jrr8 :: Gameboy -> Gameboy
jrr8 gb =
  gb & gbCPU . cpuPC +~ 2 -- 1 opcode, 1 lookup, THEN relative jumping
    & ( if r8 < 0
          then gbCPU . cpuPC -~ fromIntegral (abs r8)
          else gbCPU . cpuPC +~ fromIntegral r8
      )
  where
    r8 = fromIntegral $ ramLookup (gb ^. gbCPU . cpuPC + 1) (gb ^. gbRAM) :: Int8

-- TODO: Consider whether implementation in terms of jrr8 is a good idea or not...
jrZr8 :: Gameboy -> Gameboy
jrZr8 gb =
  if gb ^. gbCPU . cpuFlagZ
    then jrr8 gb
    else gb & gbCPU . cpuPC +~ 2

jrNZr8 :: Gameboy -> Gameboy
jrNZr8 gb =
  if not (gb ^. gbCPU . cpuFlagZ)
    then jrr8 gb
    else gb & gbCPU . cpuPC +~ 2

jrCr8 :: Gameboy -> Gameboy
jrCr8 gb =
  if gb ^. gbCPU . cpuFlagC
    then jrr8 gb
    else gb & gbCPU . cpuPC +~ 2

jrNCr8 :: Gameboy -> Gameboy
jrNCr8 gb =
  if not (gb ^. gbCPU . cpuFlagC)
    then jrr8 gb
    else gb & gbCPU . cpuPC +~ 2

cpl :: Gameboy -> Gameboy
cpl gb =
  gb & gbCPU . cpuRegisterA %~ complement
    & gbCPU . cpuFlagN .~ True
    & gbCPU . cpuFlagH .~ True
    & gbCPU . cpuPC +~ 1

scf :: Gameboy -> Gameboy
scf gb =
  gb & gbCPU . cpuFlagC .~ True
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
    & gbCPU . cpuPC +~ 1

ccf :: Gameboy -> Gameboy
ccf gb =
  gb & gbCPU . cpuFlagC %~ not
    & gbCPU . cpuFlagN .~ False
    & gbCPU . cpuFlagH .~ False
    & gbCPU . cpuPC +~ 1