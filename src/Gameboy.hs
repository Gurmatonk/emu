{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Gameboy where

import Control.Lens
import Data.Bits (bit, complement, shiftL, shiftR, (.&.), (.|.))
import Data.Bool (bool)
import Data.Int (Int8)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
import Data.Word (Word8, Word16)
import Data.Tuple (swap)

import MCU (MCU)
import qualified MCU

data Gameboy = Gameboy
  { _gbCPU :: CPU,
    _gbPPU :: PPU,
    _gbMCU :: MCU,
    _gbIME :: Bool
  }
  deriving (Show)

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

data PPU = PPU
  { _thing :: Bool,
    _anotherThing :: Bool
  }
  deriving (Show)

makeLenses ''Gameboy
makeLenses ''CPU
makeLenses ''PPU

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
      _gbMCU = MCU.initMcu,
      _gbPPU = initPpu,
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

initPpu :: PPU
initPpu = undefined

mkWord16 ::
  -- | HI
  Word8 ->
  -- | LO
  Word8 ->
  Word16
mkWord16 hi lo = ((fromIntegral hi :: Word16) `shiftL` 8) + fromIntegral lo

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = (fromIntegral ((w .&. 0xFF00) `shiftR` 8), fromIntegral (w .&. 0x00FF))

mcuLookup :: Word16 -> Gameboy -> Word8
mcuLookup w gb = MCU.addressLookup w (gb ^. gbMCU)

mcuWrite :: Word16 -> Word8 -> Gameboy -> Gameboy
mcuWrite a w gb = gb & gbMCU .~ MCU.addressWrite a w (gb ^. gbMCU)

-- s -> (s, a) is actually State... consider!
pcLookup :: Gameboy -> (Gameboy, Word8)
pcLookup gb = (gb & gbCPU. cpuPC +~ 1, res)
  where
    res = mcuLookup (gb ^. gbCPU . cpuPC) gb